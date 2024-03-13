 server = function(input, output, session) {
  
    # Create a reactiveValues object to store the LAS files
    ## Selection and parameters
    rv <- reactiveValues(console_output = list(),
                        in_dir = NULL,
                        metadata = NULL,
                        pc1 = NULL,
                        pc2 = NULL,
                        crs = NULL,
                        resolution = NULL,
                        out_dir = NULL,
                        union_mask = NULL,
                        classified_diff = NULL,
                        results = NULL)
    
    #Server logic to load PC1 from Directory
    observeEvent(input$file1, {
      inFile <- input$file1
      if (is.null(inFile)) {
          return(NULL)
      } else {
        pc1 <- spatial_container$new(inFile$datapath)
        pc1$set_crs(rv$crs)
        rv$pc1 <- pc1
      }
        
      # Update selected objects dropdown
      updateSelectInput(session, "selected_obj", choices = c("pc1", names(rv$pc1)))
      updateSelectInput(session, "selected_obj2", choices = c("pc2", names(rv$pc2)))
      print(paste("Updated selected_obj choices: ", toString(names(rv$pc1))))
    })

    #Server logic to load PC2 from Directory
    observeEvent(input$file2, {
      inFile <- input$file2
      if (is.null(inFile)) {
          return(NULL)
      } else {
        pc2 <- spatial_container$new(inFile$datapath)
        pc2$set_crs(rv$crs)
        rv$pc2 <- pc2
      }
      # Update selected objects dropdown
      updateSelectInput(session, "selected_obj", choices = c("pc1", "pc2", names(rv)))  
      updateSelectInput(session, "selected_obj2", choices = c("pc1", "pc2", names(rv)))  
      print(paste("Updated selected_obj choices: ", toString(names(rv))))
    })
    

    #Selecting which PC is PC1
    selected_las <- reactive({
      req(input$selected_obj)
      print(paste("Current selected_obj for PC 1: ", input$selected_obj))
      rv[[input$selected_obj]]
    })

    #Selecting which PC is PC2
    selected_las2 <- reactive({ 
      req(input$selected_obj2)
      print(paste("Current selected_obj for PC 2: ", input$selected_obj2))
      rv[[input$selected_obj2]]
    })

    #Building the DTM for the first PC with Text Prompts
    observeEvent(input$dtm1, {
      req(rv, selected_las())

      tryCatch({
          new_message <- paste0("Executing task: Generate DTM1 for ", selected_las()$filename)
          rv$console_output <- c(rv$console_output, list(new_message))

          #Calling the DTM Function
          dtm <- selected_las()$to_dtm(rv$resolution)
          rv$pc1$DTM <- dtm
          #Printing the DTM statistics
          object_message <- print(selected_las()$DTM)
          rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e){
          new_message <- paste("Error in creating DTM:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$dtm2, {

      #Ensuring that the resolution and the PC are selected
      req(rv, selected_las2())

      #Pushing the text prompt for DTM2 to the console
      tryCatch({
        new_message <- paste0("Executing task: Generate DTM2 for ", selected_las2()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))
        dtm <- selected_las2()$to_dtm(rv$resolution)
        rv$pc2$DTM <- dtm

        #Printing the DTM statistics
        object_message <- print(selected_las2()$DTM)
        rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e){
          new_message <- paste("Error in creating DTM:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$chm1, {
        tryCatch({
        new_message <- paste0("Executing task: Generate CHM1 for ", selected_las()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))
        chm1 <- rv$pc1$to_chm(resolution = rv$resolution)
        rv$pc1$CHM <- chm1

        object_message <- print(selected_las()$CHM)
        rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e){
          new_message <- paste("Error in creating CHM:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$chm2, {
      tryCatch({
        new_message <- paste0("Executing task: Generate CHM2 for ", selected_las2()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))
        chm2 <- rv$pc2$to_chm(resolution = rv$resolution)
        rv$pc2$CHM <- chm2

        object_message <- print(selected_las2()$CHM)
        rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e) {
          new_message <- paste("Error in creating CHM2:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$align_chms, {
      req(selected_las()$CHM, selected_las2()$CHM, selected_las()$mask, rv)
      new_message <- "Running Raster Alignment"
      rv$console_output <- c(rv$console_output, list(new_message))
      
      tryCatch({
        processed_chm <- process_raster(rv$pc1$CHM, rv$pc2$CHM, rv$pc1$mask, rv$pc2$mask, crs = rv$crs, method = "bilinear")
        print("Processed CHM 1:")
        print(processed_chm[[1]])

        print("Processed CHM 2:")
        print(processed_chm[[2]])

        rv$pc1$CHM <- processed_chm[[1]]
        rv$pc2$CHM <- processed_chm[[2]]
        rv$union_mask <- processed_chm[[3]]

        
        new_message <- "Raster Alignment Complete"
        rv$console_output <- c(rv$console_output, list(new_message))
        
      }, error = function(e){
        new_message <- paste("Error in aligning CHMs:", e$message)
        rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    #Button logic to classify the difference between the two CHMs
    
    observeEvent(input$classify_chm, {
      # Ensure the DTMs exist and have been processed for each spatial_obj
      req(rv$pc1$CHM, rv$pc2$CHM, rv)
      new_message <- "Running Classification"
      rv$console_output <- c(rv$console_output, list(new_message))

      tryCatch({
        
        classified_diff <- CHM_diff_classify(rv$pc1$CHM, rv$pc2$CHM)
        diff_class <- terra::mask(classified_diff, rv$union_mask)

        # Save the processed raster in the rv list so it can be accessed elsewhere
        rv$classified_diff <- diff_class

        new_message <- "Classification Complete"
        rv$console_output <- c(rv$console_output, list(new_message))
        
      }, error = function(e){
        new_message <- paste("Error in Classifying CHMs:", e$message)
        rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

## Plot buttons
    
   ## Plot Terminal

    output$console_output <- renderPrint({
      lapply(rv$console_output, print)  # Display all console output messages
    })

  ## Plot Las
    observeEvent(input$plot_las, {
      output$plot3D <- renderRglwidget({
        lidR::plot(selected_las()$LPC)
        rglwidget()
      })
    })

  ##Plot Results

    observeEvent(input$plot_results, {
      output$plot2D <- renderPlot({
        req(rv$classified_diff)
        plot_stats(rv$classified_diff)
      })
    })

# initialized Leaflet
  observeEvent(rv$pc1, {
      if(!is.null(rv$pc1$mask)) {
      print(paste("The mask for PC 1 is plotted"))
      output$leafletmap <- renderLeaflet({
        initial_map(rv$pc1$mask)
      })
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


 ## Save Buttons
 
 #Saving the xyz from the PCC
 
 observeEvent(input$xyz, {
   print(paste("Executing task: Convert to XYZ for", input$selected_las))
   selected_las()$to_xyz()
   print(paste("Finished executing task: Convert to XYZ for", input$selected_las))
 })
 
 observeEvent(input$save_las, {
   filepath <- selected_las()$filepath
   filename <- basename(filepath)
   out_dir <- normalizePath(rv$out_dir)
   path <- paste0(out_dir, "/", filename, ".laz")
   selected_las()$save_las(path)
 })
 
 observeEvent(input$save_dtm, {
   filepath <- selected_las()$filepath
   filename <- basename(filepath)
   out_dir <- normalizePath(rv$out_dir)
   path <- paste0(out_dir, "/", filename, "_DTM.tif")
   selected_las()$save_dtm(path)
 })
 
 observeEvent(input$save_chm, {
   filepath <- selected_las()$filepath
   filename <- basename(filepath)
   out_dir <- normalizePath(rv$out_dir)
   path <- paste0(out_dir, "/", filename, "_CHM.tif")
   selected_las()$save_chm(path)
 })
 
 observeEvent(input$save_mask, {
   out_dir <- normalizePath(rv$out_dir)
   path <- paste0(out_dir, "/", selected_las(), "_mask.shp")
   selected_las()$save_mask(path)
 })
 
 observeEvent(input$save_pc_1, {
   out_dir <- normalizePath(rv$out_dir)
   path <- paste0(out_dir, "pc1.RData")
   selected_las()$save_pc(path)
 })
 
 observeEvent(input$save_pc_1, {
   out_dir <- normalizePath(rv$out_dir)
   path <- paste0(out_dir, "pc1.RData")
   selected_las2()$save_pc(path)
 })
}