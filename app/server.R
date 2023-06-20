shiny::server = function(input, output, session) {
    las_objects <- reactiveValues(data = list(), names = list())

    object_status <- reactive({
      status_list <- sapply(las_objects$data, function(obj) {
        c(LAS = if(!is.null(obj$LPC)) "Present" else "Absent",
          CHM = if(!is.null(obj$CHM)) "Present" else "Absent",
          DTM = if(!is.null(obj$DTM)) "Present" else "Absent",
          mask = if(!is.null(obj$mask)) "Present" else "Absent")
      })
      # Convert the status_list (a matrix) to a data frame
        status_df <- as.data.frame(status_list)
        # Transpose the data frame so that the PC_obj are the column names
        # and LAS, CHM, DTM, and Mask are the rows
        status_df <- t(status_df)
        # Set the column names to the names of the PC_obj
        names(status_df) <- c("LAS", "CHM", "DTM", "Mask")
        status_df
      })

  output$status_table <- renderTable({
    req(object_status())
    object_status()
  })
    observeEvent(input$file, {
      inFile <- input$file
      if (is.null(inFile)) return(NULL)
      las_obj <- PC_obj$new(inFile$datapath)
      las_objects$data <- append(las_objects$data, list(las_obj))
      las_objects$names <- append(las_objects$names, inFile$name)
      updateSelectInput(session, "selected_obj", choices = las_objects$names)
    })


    observeEvent(input$mask, {
      for (name in names(las_objects$data)) {
        showNotification(paste("Executing task: Get Mask for", name))
        future::future_promise({
          las_objects$data[[name]]$get_mask()
        }) %...>% {
          showNotification(paste("Finished executing task: Get Mask for", name))
      }
    }
  })

    observeEvent(input$xyz, {
      for (name in names(las_objects$data)) {
        showNotification(paste("Executing task: Convert to XYZ for", name))
        path <- input$dir$datapath
        path <- paste0(path, "/", name, ".xyz")
        las_objects$data[[name]]$to_xyz(path)
        showNotification(paste("Finished executing task: Convert to XYZ for", name))
      }
    })

    observeEvent(input$dtm, {
      for (name in names(las_objects$data)) {
        showNotification(paste("Executing task: Generate DTM for", name))
        future::future_promise({
          las_objects$data[[name]]$to_dtm(resolution = 0.5)
        }) %...>% {
        showNotification(paste("Finished executing task: Generate DTM for", name))
      }
    }
  })

    observeEvent(input$chm, {
      for (name in names(las_objects$data)) {
        showNotification(paste("Executing task: Generate CHM for", name))
        future::future_promise({
          las_objects$data[[name]]$to_chm(res = 0.5)
        }) %...>% {
        showNotification(paste("Finished executing task: Generate CHM for", name))
        las_obj$CHM <- clamp  # Overwrite the existing CHM with the new one
      }
    }
  })

    observeEvent(input$submit_directory, {
    path <- input$directory
    })

    observeEvent(input$save_las, {
      # Specify a path here
      for (name in names(las_objects$data)) {
        name <- strsplit(name, ".")[1]
        path <- input$dir$datapath
        path <- paste0(path, "/", name, ".laz")
        las_objects$data[[name]]$save_las(path)
      }
    })

    observeEvent(input$save_dtm, {
      # Specify a path here
      for (name in names(las_objects$data)) {
        name <- strsplit(name, ".")[1]
        path <- input$dir$datapath
        path <- paste0(path, "/", name, ".tif")
        las_objects$data[[name]]$save_dtm(path)
      }
    })

    observeEvent(input$plot_las, {
    output$plot3D <- renderRglwidget({
        selected_index <- which(las_objects$names == input$selected_obj)
        las_obj <- las_objects$data[[selected_index]]
        lidR::plot(las_obj$LPC)
        rglwidget()
      })
  })
    observeEvent(input$plot_chm, {
      selected_index <- which(las_objects$names == input$selected_obj)
      las_obj <- las_objects$data[[selected_index]]
      plot(las_obj$CHM)
      plotOutput("plot2D")
    })

    observeEvent(input$plot_dtm, {
      selected_index <- which(las_objects$names == input$selected_obj)
      las_obj <- las_objects$data[[selected_index]]
      plot(las_obj$DTM)
      plotOutput("plot2D")
    })

    observeEvent(input$save_chm, {
    for (name in names(las_objects$data)) {
      name <- strsplit(name, ".")[1]
      path <- input$dir$datapath
      path <- paste0(path, "/", name, "CHM.tif")
      las_objects$data[[name]]$save_chm(path)
      }
    })
  }