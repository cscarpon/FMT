 server = function(input, output, session) {

    # Create a reactiveValues object to store the LAS files
    ## Selection and parameters
    rv <- reactiveValues(console_output = list(),
                        in_dir = NULL,
                        out_dir = NULL,
                        metadata = NULL,
                        sc1 = NULL,
                        sc2 = NULL,
                        footprints = NULL,
                        crs = NULL,
                        resolution = NULL,
                        processing = NULL,
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
      
      # Filter for .laz files
      laz_names <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$ || \\.las$", file_path)) %>%
        dplyr::pull(file_name)
      
      # Update the dropdown for selecting the source point cloud
      updateSelectInput(session, "selected_source", choices = laz_names)
      updateSelectInput(session, "selected_target", choices = laz_names)
      
      # Filter for footprint files
      shp_names <- rv$metadata %>%
        dplyr::filter(grepl("\\.shp$", file_path)) %>%
        dplyr::pull(file_name)
      
      updateSelectInput(session, "selected_buildings", choices = shp_names)
      
    })
    
    observeEvent(input$PC_confirm, {
      
      showModal(modalDialog("Initializing LAS and LAS Index", footer = NULL))
      # Ensure the selections are made
      req(input$selected_source, input$selected_target, input$selected_buildings, rv$metadata)
      
      laz_data <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$|\\.las$", file_path)) %>%
        dplyr::select(file_path, file_name)
      
      source_laz <- input$selected_source
      target_laz <- input$selected_target
      buildings_shape <- input$selected_buildings
      
      # Match source_laz and target_laz to their corresponding paths
      source_path <- laz_data %>%
        dplyr::filter(file_name == source_laz) %>%
        dplyr::pull(file_path)
      
      # Match laz_name1 and laz_name2 to their corresponding paths
      target_path <- laz_data %>%
        dplyr::filter(file_name == target_laz)  %>%
        dplyr::pull(file_path)
      
      # Filter for footprint files
      build_path <- rv$metadata %>%
        dplyr::filter(file_name == buildings_shape) %>%
        dplyr::pull(file_path)

      # Retrieve the file paths from the selections
      las_path1 <- normalizePath(source_path, winslash = "/")
      las_path2 <- normalizePath(target_path, winslash = "/")
      build_path <- normalizePath(build_path, winslash = "/")
      
      # Adding the footprints to the RV
      
      rv$footprints <- sf::st_read(build_path)
  
      showModal(modalDialog("Initializing LAS and Index for PC 1", footer = NULL))
      
      # Process the source point cloud
      if (!is.null(las_path1)) {
        sc1 <- spatial_container$new(as.character(las_path1))
        sc1$set_crs(rv$crs)
        rv$sc1 <- sc1
      }
      
      new_message <- capture_output(print(rv$sc1$LPC))
      add_message(new_message, rv)
      
      showModal(modalDialog("Initializing LAS and Index for PC 2", footer = NULL))
      
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
      
      output$leafletmap <- renderLeaflet({
        displayIndex(sc1$index)
      })
      
      removeModal()
      
     })
    # Server logic to to create masks for both PCs on the source and target point clouds
    
    observeEvent(input$run_mask, {
      req(rv$sc1, rv$sc2)
      
      add_message("Generating a mask for Source and Target", rv)
      
      showModal(modalDialog("Creating mask for Source PC", footer = NULL))
      
      # Generate mask for source point cloud
      
      mask_source <- mask_pc(rv$sc1$LPC)
      rv$sc1$mask <- sf::st_transform(mask_source, crs = sf::st_crs(rv$sc1$LPC))
      
      showModal(modalDialog("Creating mask for Target PC", footer = NULL))
      
      mask_target <- mask_pc(rv$sc2$LPC)
      rv$sc2$mask <- sf::st_transform(mask_source, crs = sf::st_crs(rv$sc2$LPC))
      
      removeModal()
    })
    
    # Server logic to Denoise both PCs on the source and target point clouds
    
    observeEvent(input$run_denoise, {
      req(rv$sc1, rv$sc2, rv$footprints)
      
      add_message("Running Denoising on Source", rv)
      
      showModal(modalDialog("Running Denoising on Source", footer = NULL))
      
      tryCatch({
        
        noiseless_source <- noise_filter_buildings(rv$sc1$LPC,rv$sc1$mask,rv$footprints)
        
        rv$sc1$LPC <- noiseless_source

      }, error = function(e) {
        new_message <- paste0("An error occurred: ", e$message)
        add_message(new_message, rv)
      })
      
      add_message("Running Denoising on Target", rv)
      
      showModal(modalDialog("Running Denoising on Target", footer = NULL))
      
      tryCatch({
        
        noiseless_target <- noise_filter_buildings(rv$sc2$LPC, rv$sc2$mask, rv$footprints)
        
        rv$sc2$LPC <- noiseless_target
        
      }, error = function(e) {
        new_message <- paste0("An error occurred: ", e$message)
        add_message(new_message, rv)
      })
      
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
        updateSelectInput(session, "selected_target_raster", choices = c("DTM" = "sc2$DTM"))
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
    
    observeEvent(input$selected_processing, {
      req(input$selected_processing != "")  # Ensure valid input
      
      new_message <- paste0("Raster processing selected: ", input$selected_processing)
      add_message(new_message, rv)
      
      # Perform further processing based on the selected raster type
      rv$processing <- input$selected_processing
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
      
      # Filter for footprint files
      shp_names <- rv$metadata %>%
        dplyr::filter(grepl("\\.shp$", file_path)) %>%
        dplyr::pull(file_name)
      
      updateSelectInput(session, "selected_buildings", choices = shp_names)
      
    })
    

    observeEvent(input$align_rasters, {
      req(rv$sc1, rv$sc2, rv$processing)
      new_message <- "Running Raster Alignment"
      add_message(new_message, rv)
      
      
      if (rv$processing == "CHM") {
      
      tryCatch({
        aligned_chm <- process_raster(source = rv$sc1$CHM_raw, target = rv$sc2$CHM_raw, source_mask = rv$sc1$mask, target_mask = rv$sc2$mask, method = "bilinear")
        
        rv$source_raster <- aligned_chm[[1]]
        rv$target_raster <- aligned_chm[[2]]
        rv$union_mask <- aligned_chm[[3]]

        
        new_message <- "Raster Alignment Complete"
        add_message(new_message, rv)
        
      }, error = function(e){
        new_message <- paste("Error in aligning CHMs:", e$message)
        add_message(new_message, rv)
      })
      } else if (rv$processing == "DTM") {
        
        tryCatch({
          aligned_dtm <- process_raster(source = rv$sc1$DTM_raw, target = rv$sc2$DTM_raw, source_mask = rv$sc1$mask, target_mask = rv$sc2$mask, method = "bilinear")
          
          rv$source_raster <- aligned_dtm[[1]]
          rv$target_raster <- aligned_dtm[[2]]
          rv$union_mask <- aligned_dtm[[3]]
          
          new_message <- "Raster Alignment Complete"
          add_message(new_message, rv)
          
        }, error = function(e){
          new_message <- paste("Error in aligning DTMs:", e$message)
          add_message(new_message, rv)
        })
      }
    })

    #Button logic to classify the difference between the two CHMs
    
    observeEvent(input$classify_raster, {
      # Ensure the DTMs exist and have been processed for each spatial_obj
      req(rv$source_raster, rv$target_raster, rv$union_mask, rv$footprints, rv$resolution, rv$crs)
      new_message <- "Running Classification"
      add_message(new_message, rv)

      tryCatch({
        
        # Create a new raster with the same extent as SB_Change and 1m resolution
        template_raster <- terra::rast(extent = terra::ext(rv$source_raster), resolution = rv$resolution)
        

        # Step 3: Rasterize the building polygons onto the new 1m raster grid
        buildings_raster <- terra::rasterize(terra::vect(rv$footprints), template_raster, background = NA)
        
        terra::crs(buildings_raster) <- terra::crs(rv$source_raster)
        
        # Step 4: Set the overlapping raster cells in SB_Change to 0 where buildings exist
        rv$source_raster[!is.na(buildings_raster)] <- 0
        rv$target_raster[!is.na(buildings_raster)] <- 0
        
        classified_diff <- diff_classify(rv$source_raster, rv$target_raster)
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
          displayMap(rv$sc1$DTM, rv$sc1$CHM, rv$classified_diff, rv$union_mask)
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
          addLegend(pal = colorNumeric("magma", domain = values(rv$sc1$CHM), na.color = "transparent"), 
                    values = values(rv$sc1$CHM), position = "bottomright", title = "Canopy Height Model (m)", 
                    layerId = "chmLegend", opacity = 1)
      } else if (rv$current_legend == "Diff") {
        leafletProxy("leafletmap") %>% clearControls() %>%
          addLegend(colors = c("darkorange", "orange", "lightgrey", "lightgreen", "darkgreen"), 
                    labels = c("< -10", "-10 to -0.5", "-0.5 to 0.5", "0.5 to 10", "> 10"), 
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
