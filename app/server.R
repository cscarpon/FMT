server = function(input, output, session) {

## Selection and parameters

      # Create a reactiveValues object to store the LAS files
      rv <- reactiveValues(console_output = list())

      #Server logic to accept the Resolution, CRS, and Output Directory
      observeEvent(input$anotherEvent, {
        # Make sure the values are set before using them
        req(rv$resolution, rv$crs, rv$out_dir)
        # Use rv$resolution, rv$crs, rv$out_dir here
      })


      #Server logic to confirm the Resolution, CRS, and Output Directory inputs
      observeEvent(input$confirm, {
        rv$resolution <- input$resolution
        rv$crs <- input$crs
        rv$out_dir <- input$out_dir
        # Print out the values to the console for debugging
        print(paste0("Resolution: ", rv$resolution))
        print(paste0("CRS: ", rv$crs))
        print(paste0("Output directory: ", rv$out_dir))
      })

      #Server logic to load PC1 from Directory
        observeEvent(input$file1, {
        inFile <- input$file1
        if (is.null(inFile)) {
            return(NULL)
        } else if (tools::file_ext(inFile$datapath) == "rdata") {
          load(inFile$datapath)
          rv$pc1 <- pc_obj
        } else {
          rv$pc1 <- pc_obj$new(inFile$datapath)
        }
          
        # Update selected objects dropdown
        updateSelectInput(session, "selected_obj", choices = c("pc1", names(rv)))
        updateSelectInput(session, "selected_obj2", choices = c("pc1", names(rv)))
        print(paste("Updated selected_obj choices: ", toString(names(rv))))
      })

      #Server logic to load PC2 from Directory
      observeEvent(input$file2, {
        inFile <- input$file2
        if (is.null(inFile)) {
            return(NULL)
        } else if (tools::file_ext(inFile$datapath) == "rdata") {
          load(inFile$datapath)
          rv$pc2 <- pc_obj
        } else {
          rv$pc2 <- pc_obj$new(inFile$datapath)
        }
        # Update selected objects dropdown
        updateSelectInput(session, "selected_obj", choices = c("pc1", "pc2", names(rv)))  # Added pc3 here
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

      #Saving the xyz from the PCC
      observeEvent(input$xyz, {
        print(paste("Executing task: Convert to XYZ for", input$selected_las))
        selected_las()$to_xyz()
        print(paste("Finished executing task: Convert to XYZ for", input$selected_las))
      })

      #Button logic to Align both CHMs from each PC
      observeEvent(input$align_chms, {
        # Ensure the CHMS exist and have been processed for each pc_obj
        req(selected_las()$CHM, selected_las2()$CHM, selected_las2()$mask, rv)
        print(paste("Running Pre-Processing"))
        processed_chm <- process_raster(selected_las()$CHM, selected_las2()$CHM, selected_las()$mask, selected_las2()$mask, crs = rv$crs, method = "bilinear")
        print(paste("Pre-Processing Complete"))
        # Save the processed raster in the rv list so it can be accessed elsewhere
        selected_las()$CHM <- processed_chm[[1]]
        selected_las2()$CHM <- processed_chm[[2]]
        rv$classified_diff <- processed_chm[[3]]
      })

      #Button logic to classify the difference between the two CHMs
      observeEvent(input$classify_chm, {
        # Ensure the DTMs exist and have been processed for each pc_obj
        req(selected_las()$CHM, selected_las2()$CHM)
        print(paste("Running Classification"))
        classified_diff <- CHM_diff_classify(selected_las()$CHM, selected_las2()$CHM)
        diff_class <- terra::mask(classified_diff, terra::vect(rv$diff_mask))
        print(paste("Classification Complete"))
        # Save the processed raster in the rv list so it can be accessed elsewhere
        rv$classified_diff <- diff_class
      })

      #Building the DTM for the first PC with Text Prompts
      observeEvent(input$dtm1, {
        req(rv$resolution, selected_las())

        #Building the messages to push to the console
        new_message <- paste0("Executing task: Generate DTM1 for ", selected_las()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))

        #Calling the DTM Function
        selected_las()$to_dtm(rv$resolution)  # use rv$resolution here

        #Printing the DTM statistics
        object_message <- print(selected_las()$DTM)
        rv$console_output <- c(rv$console_output, list(object_message))


        #Printing the closing message to the console.
        new_message <- paste("Finished executing task: Generate DTM1 for", selected_las()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))
      })

      observeEvent(input$dtm2, {

        #Ensuring that the resolution and the PC are selected
        req(rv$resolution, selected_las2())

        #Pushing the text prompt for DTM2 to the console
        new_message <- paste0("Executing task: Generate DTM2 for ", selected_las2()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))
        selected_las2()$to_dtm(rv$resolution)  # use rv$resolution here


        #Printing the DTM statistics
        object_message <- print(selected_las2()$DTM)
        rv$console_output <- c(rv$console_output, list(object_message))

        #Printing the closing message to the console.
        new_message <- paste("Finished executing task: Generate DTM2 for", selected_las2()$filename)
        rv$console_output <- c(rv$console_output, list(new_message))
      })

      observeEvent(input$chm1, {
        print(paste("Executing task: Generate CHM for", selected_las()$filename))
        selected_las()$to_chm(resolution = rv$resolution)
        print(paste("Finished executing task: Generate CHM for", selected_las()$filename))
      })

      observeEvent(input$chm2, {
        print(paste("Executing task: Generate CHM for", selected_las2()$filename))
        selected_las2()$to_chm(resolution = rv$resolution)
        print(paste("Finished executing task: Generate CHM for", selected_las2()$filename))
      })

      observeEvent(input$raster_statistics, {
        # Ensure the DTMs exist and have been processed for each pc_obj
        req(rv$classified_diff)
        print(paste("Running Raster Statistics"))
        stats <- raster_stats(rv$classified_diff)
        print(paste("Raster Statistics Complete"))
        # Save the processed raster in the rv list so it can be accessed elsewhere
        rv$results <- stats
      })

## Save Buttons

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

#Plot Terminal

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
          output$plot2D <- plotOutput({
            req(rv$results)
            df <- rv$results
            df$Category <- factor(df$value,
                      levels = 1:5,
                      labels = c("Loss - Greater than 10m",
                                 "Loss - from 2.5m to 10m",
                                 "No distinguishable change",
                                 "Gain - from 2.5m to 10m",
                                 "Gain - Greater than 10m"))
            results_plot <- ggplot(df, aes(x = Category, y = area, fill = Category)) +
                              geom_bar(stat = "identity") +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                              labs(x = "Category", y = "Area (m^2)", fill = "Category") +
                              theme_bw() +
                              ggtitle("Area of each height change category")
    plot(results_plot)
          })
        })

## Plot Leaflet

    observeEvent(input$plot_leaf, {
      output$leafletmap <- renderLeaflet({
        print(paste("Executing task: Plot Leaflet. Please wait as the rasters are converted to its web format"))
        displayMap(selected_las()$DTM, selected_las()$CHM, rv$classified_diff, rv$diff_mask)
        print(paste("Map has been plotted. Please check the Leaflet tab"))
      })
    })

## Initialized Leaflet

  observeEvent(rv$pc1, {
     if(!is.null(rv$pc1)) {
      print(paste("The mask for PC 1 is plotted"))
      output$leafletmap <- renderLeaflet({
        mask <- if(!is_empty_sfc(rv$pc1$mask)) sf::st_transform(rv$pc1$mask, 4326) else NULL
        pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL

        m <- leaflet() %>%
                addTiles()
            if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
            m <- addPolygons(m, data = mask, color = "red", group = "Mask")
          }
          m <- addLayersControl(
            m,
            overlayGroups = c("Mask"),
            options = layersControlOptions(collapsed = FALSE)
          )
          return(m)
    })
   }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}