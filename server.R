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
                        current_legend = NULL,
                        out_num = 0)
    
    #Server logic to accept the directories and plot the metadata
    # Non-reactive value to store the data directory path
    # Define the default paths
    data_default <- paste0(getwd(), "/data/")
    save_drive <- paste0(getwd(), "/saves/")
    
    # Call the function to create directories if they don't exist
    create_directories(data_default, save_drive)
    
    rv <- reactiveValues(console_output = list(messages = "Welcome to FMT"))
    
    output$console_output <- renderUI({
      lapply(seq_along(rv$console_output), function(i) {
        div(
          style = "border: 1px solid #ccc; padding: 5px; margin: 5px; background-color: #f9f9f9;",
          HTML(paste0("<strong>Message ", i, ":</strong><br>", rv$console_output[[i]]))
        )
      })
    })
    
    # Upload files
    observeEvent(input$upload_file, {
      req(input$upload_file, rv)
      save_path <- file.path(paste0(getwd(), "/data"), input$upload_file$name)
      file.copy(input$upload_file$datapath, save_path)
      
     add_message(paste0("File uploaded to: ", save_path), rv)
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
      a
      # Filter for .laz files
      laz_names <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$ || \\.las$", file_path)) %>%
        dplyr::pull(file_name)
      
      # Update the dropdown for selecting the source point cloud
      updateSelectInput(session, "selected_source", choices = laz_names)
      updateSelectInput(session, "selected_target", choices = laz_names)
    })
    
    observeEvent(input$PC_confirm, {
      
      showModal(modalDialog("Initializing LAS and creating masks", footer=NULL))
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
      showModal(modalDialog("Initializing LAS and creating mask for PC 1", footer=NULL))
      
      # Process the source point cloud
      if (!is.null(las_path1)) {
        sc1 <- spatial_container$new(as.character(las_path1))
        sc1$set_crs(rv$crs)
        rv$sc1 <- sc1
      }
      
      new_message <- capture_output(print(rv$sc1$LPC))
      add_message(new_message, rv)
      
      showModal(modalDialog("Initializing LAS and creating mask for PC 2", footer = NULL))
      
      add_message("Initializing the Target point cloud", rv)
      
      # Process the target point cloud
      if (!is.null(las_path2)) {
        sc2 <- spatial_container$new(as.character(las_path2))
        sc2$set_crs(rv$crs)
        rv$sc2 <- sc2  
      }
      new_message <- capture_output(print(rv$sc2$LPC))
      add_message(new_message, rv)
      
      updateSelectInput(session, "io_obj", choices = c("sc1" = "sc1", "sc2" = "sc2"))
      
      removeModal()

    })
    
    # Server logic to run ICP on the source and target point clouds
    
    observeEvent(input$run_icp, {
      req(rv$sc1, rv$sc2)
      
      add_message("Running ICP on Source and Target", rv)
      
      showModal(modalDialog("Running ICP on Source and Target", footer= NULL))
      
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
          add_message(new_message, rv)
        } else {
          new_message <- "Alignment failed."
          add_message(new_message, rv)
        }
      }, error = function(e) {
        new_message <- paste0("An error occurred: ", e$message)
        add_message(new_message, rv)
      })
        removeModal()
    })

    #Building the DTM for the first PC with Text Prompts
    observeEvent(input$dtm1, {
      
      req(rv$sc1, rv$resolution)
      tryCatch({
  
          #Calling the DTM Function
          rv$sc1$to_dtm(rv$resolution)
          
          #Printing the DTM statistics
          object_message <- capture_output(print(rv$sc1$DTM))
          add_message(object_message, rv)
        }, error = function(e){
          new_message <- paste("Error in creating DTM:", e$message)
          add_message(new_message, rv)
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
        object_message <- capture_output(print(rv$sc2$DTM))
        add_message(object_message, rv)
      }, error = function(e){
        new_message <- paste("Error in creating DTM:", e$message)
        add_message(new_message, rv)
      })
    })

    observeEvent(input$chm1, {
        
        req(rv$sc1, rv$resolution)
      
        new_message <- paste0("Generating CHM for Source")
        add_message(new_message, rv)
        
        tryCatch({
        
        rv$sc1$to_chm(resolution = rv$resolution)

        object_message <- capture_output(print(rv$sc1$CHM))
        add_message(object_message, rv)
        }, error = function(e){
          new_message <- paste("Error in creating CHM:", e$message)
          add_message(new_message, rv)
      })
    })

    observeEvent(input$chm2, {
    
      req(rv$sc2, rv$resolution)
      new_message <- paste0("Generating CHM for Target")
      add_message(new_message, rv)
    
      
      tryCatch({
        
        rv$sc2$to_chm(resolution = rv$resolution)
        object_message <- capture_output(print(rv$sc2$CHM))
        add_message(object_message, rv)
        }, error = function(e) {
          new_message <- paste("Error in creating CHM2:", e$message)
          add_message(new_message, rv)
      })
    })

    observeEvent(input$align_chms, {
      req(rv$sc1, rv$sc2)
      new_message <- "Running Raster Alignment"
      add_message(new_message, rv)
      
      tryCatch({
        aligned_chm <- process_raster(rv$sc1$CHM_raw, rv$sc2$CHM_raw, source_mask = rv$sc1$mask, target_mask = rv$sc2$mask, method = "bilinear")
        
        rv$source_chm <- aligned_chm[[1]]
        rv$target_chm <- aligned_chm[[2]]
        rv$union_mask <- aligned_chm[[3]]

        
        new_message <- "Raster Alignment Complete"
        add_message(new_message, rv)
        
      }, error = function(e){
        new_message <- paste("Error in aligning CHMs:", e$message)
        add_message(new_message, rv)
      })
    })

    #Button logic to classify the difference between the two CHMs
    
    observeEvent(input$classify_chm, {
      # Ensure the DTMs exist and have been processed for each spatial_obj
      req(rv$source_chm, rv$target_chm, rv$union_mask)
      new_message <- "Running Classification"
      add_message(new_message, rv)

      tryCatch({
        
        classified_diff <- CHM_diff_classify(rv$source_chm, rv$target_chm)
        diff_class <- terra::mask(classified_diff, rv$union_mask)

        # Save the processed raster in the rv list so it can be accessed elsewhere
        rv$classified_diff <- diff_class

        new_message <- "Classification Complete"
        add_message(new_message, rv)
        
      }, error = function(e){
        new_message <- paste("Error in Classifying CHMs:", e$message)
        add_message(new_message, rv)
      })
    })
    
    
  ## Plot Source LAS
    observeEvent(input$plot_source, {
      output$plot3D <- rgl::renderRglwidget({
        req(rv$sc1$LPC)
        lidR::plot(rv$sc1$LPC, bg = "white")
        rglwidget()
      })
    })
    
    ## Plot Target LAS
    
    observeEvent(input$plot_target, {
      output$plot3D <- rgl::renderRglwidget({
        req(rv$sc2$LPC)
        lidR::plot(rv$sc2$LPC, bg = "white")
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
      tryCatch({
        output$leafletmap <- renderLeaflet({
          displayMap(rv$sc1$DTM, rv$source_chm, rv$classified_diff, rv$union_mask)
        })
      }, error = function(e) {
        print(paste("An error occurred while plotting the map:", e$message))
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
    req(selected_las(), rv$out_dir)  
    las <- selected_las()  
    
    if (!is.null(las$filepath)) {
      filepath <- las$filepath
      filename <- basename(filepath)
      filesplit <- strsplit(filename, "\\.")[[1]]
      final_name <- filesplit[1]
      out_dir <- rv$out_dir
      path <- paste0(out_dir, "/", final_name, ".laz")
      las$save_las(path)
      print(paste("LAS file saved to:", path))
    } else {
      print("No file path found for saving LAS.")
    }
  })
 
  observeEvent(input$save_dtm, {
    
    # Ensure selected_las() is not NULL
    req(selected_las(), rv$out_dir)
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- rv$out_dir
    path <- paste0(out_dir, "/", final_name, "_DTM.tif")
    
    selected_las()$save_dtm(path)
    print(paste("DTM file saved to:", path))
  })
 
  observeEvent(input$save_chm, {
    
    # Ensure selected_las() is not NULL
    req(selected_las(), rv$out_dir)  
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- rv$out_dir
    path <- paste0(out_dir, "/", final_name, "_CHM.tif")
    
    selected_las()$save_chm(path)
    print(paste("CHM file saved to:", path))
  })
 
  observeEvent(input$save_mask, {
    
    # Ensure selected_las() is not NULL
    
    req(selected_las(), rv$out_dir)  
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- rv$out_dir
    
    path <- paste0(out_dir, "/", final_name, "_mask.shp")
    selected_las()$save_mask(path)
    print(paste("Mask saved to:", path))
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("all_files", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      req(rv$out_dir)
      save_dir <- rv$out_dir
      file_zip <- list.files(save_dir, full.names = TRUE)
      zip::zipr(file, file_zip)
    },
    contentType = "application/zip"
  )
  
  # Clean up uploaded files when the session ends, excluding base files
  session$onSessionEnded(function() {
    req(rv$out_dir)
    base_files <- c("TTP_2015.laz", "TTP_2019.laz")
    uploaded_files <- list.files(rv$out_dir, full.names = TRUE)
    lapply(uploaded_files, function(file) {
      if (file.exists(file) && !basename(file) %in% base_files) {
        file.remove(file)
      }
    })
  })
}
