 server = function(input, output, session) {

    # Create a reactiveValues object to store the LAS files
    ## Selection and parameters
    rv <- reactiveValues(console_output = list(),
                        in_dir = NULL,
                        out_dir = NULL,
                        metadata = NULL,
                        sc1 = NULL,
                        sc2 = NULL,
                        crs = NULL,
                        resolution = NULL,
                        source_chm = NULL,
                        target_chm = NULL,
                        union_mask = NULL,
                        classified_diff = NULL,
                        results = NULL)
    
    #Server logic to accept the directories and plot the metadata
    
    observeEvent(input$confirm, {
        # Update the reactive values for in_dir, out_dir, resolution, and crs
        
        # input dir
        in_input <- input$in_dir
        rv$in_dir <- as.character(in_input)
        
        #output dir
        out_input <- input$out_dir
        rv$out_dir <- as.character(out_input)
        
        #raster resolution
        res <- as.integer(input$resolution)
        rv$resolution <- res
        
        # spatial object crs
        crs <- as.integer(input$crs)
        rv$crs <- crs
        
        req(rv$in_dir)  # Make sure the input directory is set

        mo_dir <- mo$new(rv$in_dir)
        rv$metadata <- mo_dir$metadata
        
        #plot the metadata
        
        output$plotmeta <- renderTable({
          req(rv$metadata)
          rv$metadata
        })
      })
    
    ##Server logic to load source PC from Directory
    observeEvent(input$confirm, {
      req(rv$metadata)  # Ensure metadata is loaded
      
      # Filter for .laz files
      laz_paths <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$", file_path)) %>%
        dplyr::pull(file_path)
      
      # Update the dropdown for selecting the source point cloud
      updateSelectInput(session, "selected_source", choices = laz_paths)
      updateSelectInput(session, "selected_target", choices = laz_paths)
    })
    
    observeEvent(input$PC_confirm, {
      # Ensure the selections are made
      req(input$selected_source, input$selected_target)
      
      # Retrieve the file paths from the selections
      las_path1 <- input$selected_source
      las_path2 <- input$selected_target
      
      # Process the source point cloud
      if (!is.null(las_path1)) {
        sc1 <- spatial_container$new(as.character(las_path1))
        sc1$set_crs(rv$crs)
        rv$sc1 <- sc1  
      }
      
      # Process the target point cloud
      if (!is.null(las_path2)) {
        sc2 <- spatial_container$new(as.character(las_path2))
        sc2$set_crs(rv$crs)
        rv$sc2 <- sc2  
      }
    
      updateSelectInput(session, "io_obj", choices = c("sc1" = "sc1", "sc2" = "sc2"))

    })

    #Building the DTM for the first PC with Text Prompts
    observeEvent(input$dtm1, {
      
      req(rv$sc1, rv$resolution)
      tryCatch({
  
          #Calling the DTM Function
          rv$sc1$to_dtm(rv$resolution)
          
          #Printing the DTM statistics
          object_message <- print(rv$sc1$DTM)
          rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e){
          new_message <- paste("Error in creating DTM:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$dtm2, {

      #Ensuring that the resolution and the PC are selected
      req(rv$sc2, rv$resolution)

      #Pushing the text prompt for DTM2 to the console
      tryCatch({
        
        #Calling the DTM Function
        rv$sc2$to_dtm(rv$resolution)
        
        #Printing the DTM statistics
        object_message <- print(rv$sc2$DTM)
        rv$console_output <- c(rv$console_output, list(object_message))
      }, error = function(e){
        new_message <- paste("Error in creating DTM:", e$message)
        rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$chm1, {
        
        req(rv$sc1, rv$resolution)
      
        new_message <- paste0("Generating CHM for Source")
        rv$console_output <- c(rv$console_output, list(new_message))
      
      
        tryCatch({
        
        rv$sc1$to_chm(resolution = rv$resolution)

        object_message <- print(rv$sc1$CHM)
        rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e){
          new_message <- paste("Error in creating CHM:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$chm2, {
    
      req(rv$sc2, rv$resolution)
      new_message <- paste0("Generating CHM for Target")
      rv$console_output <- c(rv$console_output, list(new_message))
    
      
      tryCatch({
        
        rv$sc2$to_chm(resolution = rv$resolution)

        object_message <- print(rv$sc2$CHM)
        rv$console_output <- c(rv$console_output, list(object_message))
        }, error = function(e) {
          new_message <- paste("Error in creating CHM2:", e$message)
          rv$console_output <- c(rv$console_output, list(new_message))
      })
    })

    observeEvent(input$align_chms, {
      req(rv$sc1, rv$sc2)
      new_message <- "Running Raster Alignment"
      rv$console_output <- c(rv$console_output, list(new_message))
      
      tryCatch({
        aligned_chm <- process_raster(rv$sc1$CHM_raw, rv$sc2$CHM_raw, source_mask = rv$sc1$mask, target_mask = rv$sc2$mask, method = "bilinear")
        
        rv$source_chm <- aligned_chm[[1]]
        rv$target_chm <- aligned_chm[[2]]
        rv$union_mask <- aligned_chm[[3]]

        
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
      req(rv$source_chm, rv$target_chm, rv$union_mask)
      new_message <- "Running Classification"
      rv$console_output <- c(rv$console_output, list(new_message))

      tryCatch({
        
        classified_diff <- CHM_diff_classify(rv$source_chm, rv$target_chm)
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
    
   ## Plot Terminal

    output$console_output <- renderPrint({
      lapply(rv$console_output, print)  # Display all console output messages
    })
    
  ## Plot Source LAS
    observeEvent(input$plot_source, {
      output$plot3D <- rgl::renderRglwidget({
        req(rv$sc1$LPC)
        lidR::plot(rv$sc1$LPC)
        rglwidget()
      })
    })
    
    ## Plot Target LAS
    
    observeEvent(input$plot_target, {
      output$plot3D <- rgl::renderRglwidget({
        req(rv$sc2$LPC)
        lidR::plot(rv$sc2$LPC)
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
    
    ##Plot Webmap
    
    observeEvent(input$plot_leaf, {
      output$leafletmap <- renderLeaflet({
        req(rv$sc1)
        displayMap(rv$sc1$DTM, rv$source_chm,rv$classified_diff, rv$union_mask)
      })
    })

 ## Save Buttons
 
    
  # Selecting which SC to save
  selected_las <- reactive({
    req(input$io_obj)
    rv[[input$io_obj]]
  })
    
 #Saving the xyz from the PCC

 
  observeEvent(input$save_las, {
    req(selected_las())  
    las <- selected_las()  
    
    if (!is.null(las$filepath)) {
      filepath <- las$filepath
      filename <- basename(filepath)
      filesplit <- strsplit(filename, "\\.")[[1]]
      final_name <- filesplit[1]
      out_dir <- normalizePath(rv$out_dir)
      path <- paste0(out_dir, "/", final_name, ".laz")
      las$save_las(path)
      print(paste("LAS file saved to:", path))
    } else {
      print("No file path found for saving LAS.")
    }
  })
 
  observeEvent(input$save_dtm, {
    
    # Ensure selected_las() is not NULL
    req(selected_las())  
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- normalizePath(rv$out_dir)
    path <- paste0(out_dir, "/", final_name, "_DTM.tif")
    
    selected_las()$save_dtm(path)
    print(paste("DTM file saved to:", path))
  })
 
  observeEvent(input$save_chm, {
    
    # Ensure selected_las() is not NULL
    req(selected_las())  
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- normalizePath(rv$out_dir)
    path <- paste0(out_dir, "/", final_name, "_CHM.tif")
    
    selected_las()$save_chm(path)
    print(paste("CHM file saved to:", path))
  })
 
  observeEvent(input$save_mask, {
    
    # Ensure selected_las() is not NULL
    
    req(selected_las())  
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- normalizePath(rv$out_dir)
    
    path <- paste0(out_dir, "/", final_name, "_mask.shp")
    selected_las()$save_mask(path)
    print(paste("Mask saved to:", path))
  })
}