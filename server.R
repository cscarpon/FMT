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
                        results = NULL,
                        current_legend = NULL)
    
    #Server logic to accept the directories and plot the metadata
    # Non-reactive value to store the data directory path
    data_default <- paste0(getwd(), "/data/")
    
    # Upload files
    observeEvent(input$upload_file, {
      req(input$upload_file)
      save_path <- file.path(paste0(getwd(), "/data"), input$upload_file$name)
      file.copy(input$upload_file$datapath, save_path)
      
      rv$console_output <- c(rv$console_output, paste0("File uploaded to: ", save_path))
    })
    
    observeEvent(input$confirm, {
        # Update the reactive values for in_dir, out_dir, resolution, and crs
        
        # input dir
        in_input <- input$in_dir
        rv$in_dir <- normalizePath(in_input)
        
        #output dir
        out_input <- input$out_dir
        rv$out_dir <- normalizePath(out_input)
        
        #raster resolution
        res <- as.integer(input$resolution)
        rv$resolution <- res
        
        # spatial object crs
        crs <- as.integer(input$crs)
        rv$crs <- crs
        
        req(rv$in_dir)
        
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
      laz_names <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$ || \\.las$", file_path)) %>%
        dplyr::pull(file_name)
      
      # Update the dropdown for selecting the source point cloud
      updateSelectInput(session, "selected_source", choices = laz_names)
      updateSelectInput(session, "selected_target", choices = laz_names)
    })
    
    observeEvent(input$PC_confirm, {
      # Ensure the selections are made
      req(input$selected_source, input$selected_target, rv$metadata)
      
      laz_data <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$|\\.las$", file_path)) %>%
        dplyr::select(file_path, file_name)
      
      source_laz <- input$selected_source
      target_laz <- input$selected_target
      
      # Match source_laz and target_laz to their corresponding paths
      source_path <- laz_data %>%
        dplyr::filter(file_name == source_laz) %>%
        dplyr::pull(file_path)
      
      # Match laz_name1 and laz_name2 to their corresponding paths
      target_path <- laz_data %>%
        dplyr::filter(file_name == target_laz)  %>%
        dplyr::pull(file_path)

      # Retrieve the file paths from the selections
      las_path1 <- normalizePath(source_path, winslash = "/")
      las_path2 <- normalizePath(target_path, winslash = "/")
      
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
      
      new_message <- paste0("Point clouds loaded:",  " Source: ", las_path1, " Target: ", las_path2)
      rv$console_output <- c(rv$console_output, list(new_message))

    })
    
    # Server logic to run ICP on the source and target point clouds
    
    observeEvent(input$run_icp, {
      req(rv$sc1, rv$sc2)
      
      new_message <- paste0("Running ICP on Source and Target")
      rv$console_output <- c(rv$console_output, list(new_message))
      
      # Use the conda environment
      reticulate::use_condaenv("fmt_env", required = TRUE)
      
      # Source the Python script
      icp_module <- paste0(getwd(), "/py/icp_pdal.py")
      
      tryCatch({
        reticulate::source_python(icp_module)
        
        # Create instance of the ICP class
        icp_aligner <- pdal_icp(rv$sc1$filepath, rv$sc2$filepath)
        
        # Call the align method
        aligned_file_path <- icp_aligner$align()
        
        # Process the source point cloud
        if (!is.null(aligned_file_path)) {
          sc1 <- spatial_container$new(as.character(aligned_file_path))
          sc1$set_crs(rv$crs)
          rv$sc1 <- sc1  
        }
        
        if (!is.null(aligned_file_path)) {
          new_message <- paste0("Alignment completed successfully. Aligned file created at: ", aligned_file_path)
          rv$console_output <- c(rv$console_output, list(new_message))
        } else {
          new_message <- "Alignment failed."
          rv$console_output <- c(rv$console_output, list(new_message))
        }
      }, error = function(e) {
        new_message <- paste0("An error occurred: ", e$message)
        rv$console_output <- c(rv$console_output, list(new_message))
      })
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
        displayMap(rv$sc1$DTM, rv$source_chm, rv$classified_diff, rv$union_mask)
      })
    })
    
    observeEvent(input$leafletmap_groups, {
      legend <- NULL
      
      if ("DTM" %in% input$leafletmap_groups) {
        legend <- "DTM"
      } else if ("CHM" %in% input$leafletmap_groups) {
        legend <- "CHM"
      } else if ("Diff" %in% input$leafletmap_groups) {
        legend <- "Diff"
      }
      
      rv$current_legend <- legend
      
      if (is.null(rv$current_legend)) {
        leafletProxy("leafletmap") %>% clearControls()
      } else if (rv$current_legend == "DTM") {
        leafletProxy("leafletmap") %>% clearControls() %>%
          addLegend(pal = colorNumeric("magma", domain = values(rv$sc1$DTM), na.color = "transparent"), 
                    values = values(rv$sc1$DTM), position = "bottomright", title = "Digital Terrain Model (m)", 
                    layerId = "dtmLegend", opacity = 1)
      } else if (rv$current_legend == "CHM") {
        leafletProxy("leafletmap") %>% clearControls() %>%
          addLegend(pal = colorNumeric("magma", domain = values(rv$source_chm), na.color = "transparent"), 
                    values = values(rv$source_chm), position = "bottomright", title = "Canopy Height Model (m)", 
                    layerId = "chmLegend", opacity = 1)
      } else if (rv$current_legend == "Diff") {
        leafletProxy("leafletmap") %>% clearControls() %>%
          addLegend(colors = c("darkorange", "orange", "lightgrey", "lightgreen", "darkgreen"), 
                    labels = c("< -10", "-10 to -2.5", "-2.5 to 2.5", "2.5 to 10", "> 10"), 
                    position = "bottomright", title = "Change in Tree Height (m)", layerId = "diffLegend", opacity = 1)
      }
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
  
  # Clean up uploaded files when the session ends, excluding base files
  session$onSessionEnded(function() {
    base_files <- c("TTP_2015.laz", "TTP_2019.laz")
    uploaded_files <- list.files(data_default, full.names = TRUE)
    lapply(uploaded_files, function(file) {
      if (file.exists(file) && !basename(file) %in% base_files) {
        file.remove(file)
      }
    })
  })
}
