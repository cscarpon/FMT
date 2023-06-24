server = function(input, output, session) {

## Selection and parameters

      # Create a reactiveValues object to store the LAS files
      rv <- reactiveValues()

      observeEvent(input$confirm, {
        rv$resolution <- input$resolution
        rv$crs <- input$crs
        rv$out_dir <- input$out_dir
        # Print out the values to the console for debugging
        print(paste0("Resolution: ", rv$resolution))
        print(paste0("CRS: ", rv$crs))
        print(paste0("Output directory: ", rv$out_dir))
      })

      observeEvent(input$anotherEvent, {
        # Make sure the values are set before using them
        req(rv$resolution, rv$crs, rv$out_dir)
        # Use rv$resolution, rv$crs, rv$out_dir here
      })

      observeEvent(input$file1, {
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        rv$pc1 <- pc_obj$new(inFile$datapath)
        # Update selected objects dropdown
        updateSelectInput(session, "selected_obj", choices = c("pc1", names(rv)))
        updateSelectInput(session, "selected_obj2", choices = c("pc1", names(rv)))
        print(paste("Updated selected_obj choices: ", toString(names(rv))))
      })

      observeEvent(input$file2, {
        inFile <- input$file2
        if (is.null(inFile)) return(NULL)
        rv$pc2 <- pc_obj$new(inFile$datapath)
        # Update selected objects dropdown
        updateSelectInput(session, "selected_obj", choices = c("pc1", "pc2", names(rv)))
        updateSelectInput(session, "selected_obj2", choices = c("pc1", "pc2", names(rv)))
        print(paste("Updated selected_obj choices: ", toString(names(rv))))
      })

      selected_las <- reactive({
        req(input$selected_obj)
        print(paste("Current selected_obj for PC 1: ", input$selected_obj))
        rv[[input$selected_obj]]
      })

      selected_las2 <- reactive({
        req(input$selected_obj2)
        print(paste("Current selected_obj for PC 2: ", input$selected_obj2))
        rv[[input$selected_obj2]]
      })

      observeEvent(input$xyz, {
        print(paste("Executing task: Convert to XYZ for", input$selected_las))
        selected_las()$to_xyz()
        print(paste("Finished executing task: Convert to XYZ for", input$selected_las))
      })

      observeEvent(input$align_chms, {
        # Ensure the DTMs exist and have been processed for each pc_obj
        req(selected_las()$CHM, selected_las2()$CHM, selected_las2()$mask)
        print(paste("Running Pre-Processing"))
        processed_chm <- process_raster(selected_las()$CHM, selected_las2()$CHM, selected_las2()$mask, method = "bilinear")
        print(paste("Pre-Processing Complete"))
        # Save the processed raster in the rv list so it can be accessed elsewhere
        rv$pc1$CHM <- processed_chm
      })

      observeEvent(input$classify_chm, {
        # Ensure the DTMs exist and have been processed for each pc_obj
        req(selected_las()$CHM, selected_las2()$CHM)
        print(paste("Running Classification"))
        classified_diff <- CHM_diff_classify(selected_las()$CHM, selected_las2()$CHM)
        diff_class <- terra::mask(classified_diff, terra::vect(selected_las2()$mask))
        print(paste("Classification Complete"))
        # Save the processed raster in the rv list so it can be accessed elsewhere
        rv$classified_diff <- diff_class
      })
      observeEvent(input$dtm1, {
        req(rv$resolution, selected_las())
        print(paste("Executing task: Generate DTM for ", selected_las()$filename))  # print the filename
        selected_las()$to_dtm(rv$resolution)  # use rv$resolution here
        print(paste("Finished executing task: Generate DTM for ", selected_las()$filename))  # print the filename
      })

      observeEvent(input$dtm2, {
        req(rv$resolution, selected_las2())
        print(paste("Executing task: Generate DTM for ", selected_las2()$filename))  # print the filename
        selected_las2()$to_dtm(rv$resolution)  # use rv$resolution here
        print(paste("Finished executing task: Generate DTM for ", selected_las2()$filename))  # print the filename
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

      # observeEvent(input$dtm, {
      #   req(rv$resolution, selected_las())
      #   print(paste("Executing task: Generate DTM for ", selected_las()$filename))  # print the filename
      #   selected_las()$to_dtm(rv$resolution)  # use rv$resolution here
      #   print(paste("Finished executing task: Generate DTM for ", selected_las()$filename))  # print the filename
      # })

      # observeEvent(input$chm, {
      #   print(paste("Executing task: Generate CHM for", input$selected_las))
      #   selected_las()$to_chm(resolution = rv$resolution)
      #   print(paste("Finished executing task: Generate CHM for", input$selected_las))
      # })

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


## Leaflet Map
    observeEvent(input$plot_leaf, {
      output$leafletmap <- renderLeaflet({
        print(paste("Executing task: Plot Leaflet. Please wait as the rasters are converted to its web format"))
        dtm <- if(!is_empty(selected_las()$DTM)) terra::project(selected_las()$DTM, "EPSG:4326") else NULL
        chm <- if(!is_empty(selected_las()$CHM)) terra::project(selected_las()$CHM, "EPSG:4326") else NULL
        diff <- if(!is_empty(rv$classified_diff)) terra::project(rv$classified_diff, "EPSG:4326") else NULL
        mask <- if(!is_empty_sfc(selected_las()$mask)) sf::st_transform(selected_las()$mask, 4326) else NULL
        pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL
        
        m <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap)

        if (!is_empty(dtm)) {
          m <- addRasterImage(m, raster::raster(dtm), group = "DTM", maxBytes = Inf)
        }
        
        if (!is_empty(chm)) {
          m <- addRasterImage(m, raster::raster(chm), group = "CHM", maxBytes = Inf)
        }

        if (!is_empty(diff)) {
          # Generate the plasma palette with 6 colors (5 + 1 for NA values)
          plasma_palette <- viridis(5, option = "plasma", end = 0.9)

          # Create a color function
          chm_pal <- colorFactor(plasma_palette, na.color = "#808080", domain = 1:6)

          # Convert diff to RasterLayer
          diff_raster <- raster::raster(diff)
          
          m <- addRasterImage(m, diff_raster, colors = chm_pal, group = "CHM_Difference", maxBytes = Inf)

          m <- m %>%
            addLegend(
              position = "bottomright",
              pal = chm_pal,
              values = 1:6,
              title = "Change in Height",
              labels = c("Loss - Greater than 10m",
                        "Loss - from 2.5m to 10m",
                        "No change",
                        "Gain - from 2.5m to 10m",
                        "Gain - Greater than 10m",
                        "Out of range"),
              opacity = 1
            )
        }
        #  if (!is.null(diff)) {
          # Set NAs to a specific value
          # Here, we're setting NA values to 6
          #diff <- terra::classify(diff, cbind(NA, 6))
        # Add an additional color for the new value
        #chm_pal <- colorFactor(c("red", "orange", "white", "lightgreen", "darkgreen", "gray"), domain = 1:6)
        #  chm_pal <- colorFactor(c("red", "orange", "white", "lightgreen", "darkgreen"), domain = 1:5)
        
          # m <- addRasterImage(m, raster::raster(diff), colors = "Plasma", group = "CHM_Difference", maxBytes = Inf)

          # m <- m %>%
          #   addLegend(
          #     position = "bottomright",
          #     pal = "Plasma",
          #     values = 1:5,
          #     title = "Change in Height",
          #     labels = c("Loss - Greater than 10m",
          #               "Loss - from 2.5m to 10m",
          #               "No change",
          #               "Gain - from 2.5m to 10m",
          #               "Gain - Greater than 10m",
          #               "Out of range"),
          #     opacity = 1
          #   )
          # }
          if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
            m <- addPolygons(m, data = mask, color = "red", group = "Mask")
          }

          m <- addLayersControl(
                m,
                overlayGroups = c("DTM", "CHM", "CHM_Difference", "Mask"),
                options = layersControlOptions(collapsed = FALSE)
          )
          print(paste("Completed task: Plot Leaflet"))
          return(m)
      })
    })

  # observeEvent(input$plot_leaf, {
  #   output$leafletmap <- renderLeaflet({
  #     print(paste("Executing task: Plot Leaflet. Please wait as the raster are converted for web format")) 
  #     dtm <- if(!is_empty(selected_las()$DTM)) terra::project(selected_las()$DTM, "EPSG:4326") else NULL
  #     chm <- if(!is_empty(selected_las()$CHM)) terra::project(selected_las()$CHM, "EPSG:4326") else NULL
  #     diff <- if(!is_empty(rv$classified_diff)) terra::project(rv$classified_diff, "EPSG:4326") else NULL
  #     mask <- if(!is_empty_sfc(selected_las()$mask)) sf::st_transform(selected_las()$mask, 4326) else NULL
  #     pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL
  #     chm_pal <- colorFactor(c("red", "orange", "white", "lightgreen", "darkgreen"), domain = 1:5)
  #     m <- leaflet() %>%
  #           addTiles()
  #     if (!is.null(dtm)) {
  #       m <- addRasterImage(m, raster::raster(dtm), group = "DTM", maxBytes = Inf)
  #     }
      
  #     if (!is.null(chm)) {
  #       m <- addRasterImage(m, raster::raster(chm), group = "CHM", maxBytes = Inf)
  #     }
      
  #     if (!is.null(diff)) {
  #       m <- addRasterImage(m, raster::raster(diff), pal = chm_pal, opacity = 0.8, group = "CHM_Difference", maxBytes = Inf)
  #     }
      
  #     m <- m %>%
  #       addLegend(
  #         position = "bottomright",
  #         pal = chm_pal,
  #         values = 1:5,
  #         title = "Change in Height",
  #         labels = c("Loss - Greater than 10m",
  #                   "Loss - from 2.5m to 10m",
  #                   "No distinguishable change",
  #                   "Gain - from 2.5m to 10m",
  #                   "Gain - Greater than 10m"),
  #         opacity = 1
  #       )

  #     if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
  #       m <- addPolygons(m, data = mask, color = "red", group = "Mask")
  #     }
      
      # m <- addLayersControl(
      #   m,
      #   overlayGroups = c("DTM", "CHM", "CHM_Difference", "Mask"),
      #   options = layersControlOptions(collapsed = FALSE)
      # )
      # print(paste("Completed task: Plot Leaflet"))
      # return(m)

  #   })
  # })

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