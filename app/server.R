#"C:/Users/User/Forest_Analyser_5000/data/TTP/LAS/Clipped"


server = function(input, output, session) {

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
        updateSelectInput(session, "selected_obj", choices = names(rv))
        print(paste("Updated selected_obj choices: ", toString(names(rv))))
      })

      observeEvent(input$file2, {
        inFile <- input$file2
        if (is.null(inFile)) return(NULL)
        rv$pc2 <- pc_obj$new(inFile$datapath)
        updateSelectInput(session, "selected_obj", choices = names(rv))
        print(paste("Updated selected_obj choices: ", toString(names(rv))))
      })

      selected_las <- reactive({
        req(input$selected_obj)
        print(paste("Current selected_obj: ", input$selected_obj))
        rv[[input$selected_obj]]
      })

      # observeEvent(input$mask, {
      #   req(selected_las())
      #   print(paste("Executing task: Get Mask for", input$selected_obj))
      #   selected_las()$get_mask()
      #   print(paste("Finished executing task: Get Mask for", input$selected_obj))
      # })

      observeEvent(input$xyz, {
        print(paste("Executing task: Convert to XYZ for", input$selected_las))
        selected_las()$to_xyz()
        print(paste("Finished executing task: Convert to XYZ for", input$selected_las))
      })

      # observeEvent(input$dtm, {
      #   tryCatch({
      #     print(paste("Executing task: Generate DTM for", input$selected_las))
      #     selected_las()$to_dtm(resolution = rv$resolution)
      #     print(paste("Finished executing task: Generate DTM for", input$selected_las))
      #   }, error = function(e) {
      #     print(paste("Error occurred during DTM generation: ", e$message))
      #   })
      # })
      observeEvent(input$dtm, {
        req(rv$resolution, selected_las())
        print(paste("Executing task: Generate DTM for ", selected_las()$filename))  # print the filename
        selected_las()$to_dtm(rv$resolution)  # use rv$resolution here
        print(paste("Finished executing task: Generate DTM for ", selected_las()$filename))  # print the filename
      })

      observeEvent(input$chm, {
        print(paste("Executing task: Generate CHM for", input$selected_las))
        selected_las()$to_chm(resolution = rv$resolution)
        print(paste("Finished executing task: Generate CHM for", input$selected_las))
      })

      observeEvent(input$plot_mask, {
        output$plot2D <- renderPlot({
          plot(selected_las()$mask)
        })
      })


      observeEvent(input$plot_leaf, {
        output$leafletmap <- renderLeaflet({
          dtm <- if(!is_empty(selected_las()$DTM)) terra::project(selected_las()$DTM, "EPSG:4326") else NULL
          chm <- if(!is_empty(selected_las()$CHM)) terra::project(selected_las()$CHM, "EPSG:4326") else NULL
          mask <- if(!is_empty_sfc(selected_las()$mask)) sf::st_transform(selected_las()$mask, 4326) else NULL
          pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL
          m <- leaflet() %>%
            addTiles()

          if (!is.null(dtm)) {
            m <- addRasterImage(m, dtm, group = "DTM")
          }
          if (!is.null(chm)) {
            m <- addRasterImage(m, chm, group = "CHM")
          }
          if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
            m <- addPolygons(m, data = mask, color = "red", group = "Mask")
          }

          m <- addLayersControl(
            m,
            overlayGroups = c("DTM", "CHM", "Mask"),
            options = layersControlOptions(collapsed = FALSE)
          )

          return(m)
        })
      })


      # observeEvent(input$crs, {
      #   selected_las()$set_crs(input$crs)
      # })

      observeEvent(input$save_las, {
        name <- strsplit(selected_las()$name, ".")[[1]][1]
        path <- file.path(input$dir$datapath, paste0(name, ".laz"))
        selected_las()$save_las(path)
      })

      observeEvent(input$save_mask, {

        out_dir <- normalizePath(rv$out_dir)
        path <- paste0(out_dir, "/", selected_las(), "_mask.shp")
        selected_las()$save_mask(path)
      })

       
      observeEvent(input$save_dtm, {
        filepath <- selected_las()$filepath
        filename <- basename(filepath)
        name <- strsplit(filename, "\\.")[[1]][1]
        out_dir <- normalizePath(rv$out_dir)
        path <- file.path(out_dir, paste0(name, "_DTM.tif"))
        selected_las()$save_dtm(path)
      })

      observeEvent(input$plot_las, {
        output$plot3D <- renderRglwidget({
          lidR::plot(selected_las()$LPC)
          rglwidget()
        })
      })

      observeEvent(input$plot_chm, {
        output$plot2D <- renderPlot({
          plot(selected_las()$CHM)
        })
      })

      observeEvent(input$plot_dtm, {
        output$plot2D <- renderPlot({
          plot(selected_las()$DTM)
        })
      })

      observeEvent(input$save_chm, {
        name <- strsplit(selected_las()$name, ".")[[1]][1]
        path <- file.path(input$dir$datapath, paste0(name, "CHM.tif"))
        selected_las()$save_chm(path)
      })

}


    # rv <- reactiveValues()  # Create a reactiveValues object to store the paths to the LAS files
    # las_objects <- reactiveValues(data = list(), names = list())

    # object_status <- reactive({
    #   status_list <- sapply(las_objects$data, function(obj) {
    #     c(LAS = if(!is.null(obj$LPC)) "Present" else "Absent",
    #       CHM = if(!is.null(obj$CHM)) "Present" else "Absent",
    #       DTM = if(!is.null(obj$DTM)) "Present" else "Absent",
    #       mask = if(!is.null(obj$mask)) "Present" else "Absent")
    #   })
      # # Convert the status_list (a matrix) to a data frame
      #   status_df <- as.data.frame(status_list)
      #   # Transpose the data frame so that the pc_obj are the column names
      #   # and LAS, CHM, DTM, and Mask are the rows
      #   status_df <- t(status_df)
      #   # Set the column names to the names of the pc_obj
      #   names(status_df) <- c("LAS", "CHM", "DTM", "Mask")
      #   status_df
      # })

  # output$status_table <- renderTable({
  #   req(object_status())
  #   object_status()
  # })
  #   observeEvent(input$file, {
  #     inFile <- input$file
  #     if (is.null(inFile)) return(NULL)
  #     las_obj <- pc_obj$new(inFile$datapath)
  #     las_objects$data <- append(las_objects$data, list(las_obj))
  #     las_objects$names <- append(las_objects$names, inFile$name)
  #     updateSelectInput(session, "selected_obj", choices = las_objects$names)
  #   })


  #   observeEvent(input$mask, {
  #     for (name in names(las_objects$data)) {
  #       showNotification(paste("Executing task: Get Mask for", name))
  #       future::future_promise({
  #         las_objects$data[[name]]$get_mask()
  #       }) %...>% {
  #         showNotification(paste("Finished executing task: Get Mask for", name))
  #     }
  #   }
  # })

  #   observeEvent(input$xyz, {
  #     for (name in names(las_objects$data)) {
  #       showNotification(paste("Executing task: Convert to XYZ for", name))
  #       path <- input$dir$datapath
  #       path <- paste0(path, "/", name, ".xyz")
  #       las_objects$data[[name]]$to_xyz(path)
  #       showNotification(paste("Finished executing task: Convert to XYZ for", name))
  #     }
  #   })

  #   observeEvent(input$dtm, {
  #     for (name in names(las_objects$data)) {
  #       showNotification(paste("Executing task: Generate DTM for", name))
  #       future::future_promise({
  #         las_objects$data[[name]]$to_dtm(resolution = 0.5)
  #       }) %...>% {
  #       showNotification(paste("Finished executing task: Generate DTM for", name))
  #     }
  #   }
  # })

  #   observeEvent(input$chm, {
  #     for (name in names(las_objects$data)) {
  #       showNotification(paste("Executing task: Generate CHM for", name))
  #       future::future_promise({
  #         las_objects$data[[name]]$to_chm(res = 0.5)
  #       }) %...>% {
  #       showNotification(paste("Finished executing task: Generate CHM for", name))
  #       las_obj$CHM <- clamp  # Overwrite the existing CHM with the new one
  #     }
  #   }
  # })

  #   observeEvent(input$submit_directory, {
  #   path <- input$directory
  #   })

  #   observeEvent(input$save_las, {
  #     # Specify a path here
  #     for (name in names(las_objects$data)) {
  #       name <- strsplit(name, ".")[1]
  #       path <- input$dir$datapath
  #       path <- paste0(path, "/", name, ".laz")
  #       las_objects$data[[name]]$save_las(path)
  #     }
  #   })

  #   observeEvent(input$save_dtm, {
  #     # Specify a path here
  #     for (name in names(las_objects$data)) {
  #       name <- strsplit(name, ".")[1]
  #       path <- input$dir$datapath
  #       path <- paste0(path, "/", name, ".tif")
  #       las_objects$data[[name]]$save_dtm(path)
  #     }
  #   })

  #   observeEvent(input$plot_las, {
  #   output$plot3D <- renderRglwidget({
  #       selected_index <- which(las_objects$names == input$selected_obj)
  #       las_obj <- las_objects$data[[selected_index]]
  #       lidR::plot(las_obj$LPC)
  #       rglwidget()
  #     })
  # })
  #   observeEvent(input$plot_chm, {
  #     selected_index <- which(las_objects$names == input$selected_obj)
  #     las_obj <- las_objects$data[[selected_index]]
  #     plot(las_obj$CHM)
  #     plotOutput("plot2D")
  #   })

  #   observeEvent(input$plot_dtm, {
  #     selected_index <- which(las_objects$names == input$selected_obj)
  #     las_obj <- las_objects$data[[selected_index]]
  #     plot(las_obj$DTM)
  #     plotOutput("plot2D")
  #   })

  #   observeEvent(input$save_chm, {
  #   for (name in names(las_objects$data)) {
  #     name <- strsplit(name, ".")[1]
  #     path <- input$dir$datapath
  #     path <- paste0(path, "/", name, "CHM.tif")
  #     las_objects$data[[name]]$save_chm(path)
  #     }
  #   })
  # }