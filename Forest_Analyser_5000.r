library(shiny)
library(terra)
library(sf)
library(lidR)
library(rgl)


options(shiny.maxRequestSize = 1073741824)


PC_obj <- setRefClass(
  "las_obj",
  fields = list(
    data = "data.frame",
    LPC = "LAS",
    mask = "sf"
  ),
  methods = list(
    initialize = function(file_path = character(0)) {
        lidR::set_lidr_threads(6)
        ext <- tools::file_ext(file_path)
        if (ext == "xyz") {
            .self$data <- read.table(file_path)
            names(.self$data) <- c("X", "Y", "Z")
            .self$LPC <- LAS(.self$data)
            ground <- classify_ground(.self$LPC, algorithm = pmf(ws = 5, th = 3))
            .self$LPC@data$Classification <- ground@data$Classification
            .self$LPC@data$ReturnNumber <- 1
            .self$LPC@data$NumberOfReturns <- 1
        } else if (ext == "las" || ext == "laz") {
            .self$LPC <- lidR::readLAS(file_path)
        if (is.empty(.self$LPC)) return(NULL)
            .self$data <- data.frame(X = .self$LPC@data$X,
                                    Y = .self$LPC@data$Y,
                                    Z = .self$LPC@data$Z,
                                    Intensity = .self$LPC@data$Intensity,
                                    ReturnNumber = .self$LPC@data$ReturnNumber,
                                    NumberOfReturns = .self$LPC@data$NumberOfReturns,
                                    Classification = .self$LPC@data$Classification
                                    )
      }
    },
    set_crs = function(crs) {
      st_crs(.self$LPC) <- crs
      .self$LPC <- st_transform(.self$LPC, crs)
      .self$data <- data.frame(X = .self$LPC@data$X,
                                    Y = .self$LPC@data$Y,
                                    Z = .self$LPC@data$Z,
                                    ReturnNumber = .self$LPC@data$ReturnNumber,
                                    NumberOfReturns = .self$LPC@data$NumberOfReturns,
                                    Classification = .self$LPC@data$Classification
                                    )

    },
    get_mask = function() {
      coords <- st_as_sf(.self$LPC@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(.self$LPC))
      coords_sample <- st_sample(coords, size = (nrow(coords)*.01), type = "regular")
      buffer <- sf::st_buffer(coords_sample, 5)
      union <- sf::st_union(buffer)
      masked <- sf::st_buffer(union, -5)
      .self$mask <- masked
    },
    get_data = function() {
      return(.self$data)
    },
    get_meta = function() {
      return(.self$metadata)
    },
    get_lpc = function() {
      return(.self$LPC)
    },
    to_xyz = function(path) {
      write.table(.self$data[,c("X", "Y", "Z")], path, row.names=FALSE, col.names=FALSE, quote=FALSE, sep=" ")
    },
    to_chm = function(res = 0.5) {
      fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) }}
      w <- matrix(1, 3, 3)
      dtm <- rasterize_terrain(.self$LPC, 0.5, tin())
      nlas <- normalize_height(.self$LPC, dtm)
      chm <- lidR::rasterize_canopy(nlas, res, p2r(0.2, na.fill = tin()))
      filled <- terra::focal(chm, w, fun = fill.na)
      clamp <- terra::clamp(filled, lower = 0)

      # Return the CHM
      return(clamp)
    }
  )
)




process_raster <- function(source, target, mask_layer, method = "bilinear") {
  aligned <- FALSE
  tryCatch({
    aligned <- terra::compareGeom(source, target, stopiffalse = FALSE, tolerance = 0.1)
  }, error = function(e) {
    print(paste("compareGeom error:", e$message))
    aligned <- FALSE
  })

  if (!aligned) {
    source <- terra::resample(source, target, method = method)
    # Crop the source to match the target raster's extent.
    source <- terra::crop(source, terra::ext(target))
  }
  source <- terra::mask(source, mask_layer)
  return(source)
}

# Function to generate CHM and classify the differences
CHM_diff_classify <- function(earlier, later) {
    # Compute the difference
    diff <- later - earlier
    # Create a raster for the magnitude of change
    magnitude <- ((later - earlier) / earlier)
    # Classify the differences
    m <- c(-Inf, -10, 1,
            -10, -2.5, 2,
            -2.5, 2.5, 3,
            2.5, 10, 4,
            10, Inf, 5)
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
    diff <- list(diff_class, magnitude)
    # Write the output
    return(diff)
}



# Define the UI
ui <- fluidPage(
  titlePanel("Tree Analyser 5000"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("las_file", "Choose LAS File"),
      fileInput("raster_file", "Choose Raster File"),
      actionButton("run_las", "Process LAS"),
      actionButton("run_raster", "Process Raster"),
      selectInput("las_files", "Select LAS file to plot", choices = NULL),
      selectInput("raster_files", "Select Raster file to plot", choices = NULL),
      actionButton("plot_las", "Plot LAS"),
      actionButton("plot_raster", "Plot Raster"),
      actionButton("save_raster", "Save Raster"),
      actionButton("save_las", "Save Las")
    ),

    mainPanel(
      rglwidgetOutput("plot3d"),
      plotOutput("raster_plot")
    )
  )
)

# Define server logic required to process the data and output
server <- function(input, output, session) {

  # To store the uploaded files
  las_files <- reactiveValues(data = list())
  raster_files <- reactiveValues(data = list())

  observeEvent(input$run_las, {
    validate(need(input$las_file, 'Please upload the LAS file'))

    # Read the uploaded LAS file
    las <- readLAS(input$las_file$datapath)

    # Store the uploaded LAS file
    las_files$data[[input$las_file$name]] <- las

    # Update the selectInput choices
    updateSelectInput(session, "las_files", choices = names(las_files$data))
  })

  observeEvent(input$run_raster, {
    validate(need(input$raster_file, 'Please upload the raster file'))

    # Read the uploaded raster file
    raster <- terra::rast(input$raster_file$datapath)

    # Store the uploaded raster file
    raster_files$data[[input$raster_file$name]] <- raster

    # Update the selectInput choices
    updateSelectInput(session, "raster_files", choices = names(raster_files$data))
  })

output$plot3d <- renderRglwidget({
    # Check if the las object exists
    if (!is.null(las_files$data[[input$las_files]])) {
      plot(las_files$data[[input$las_files]])
      rglwidget()
    }
  })


  observeEvent(input$plot_las, {
    validate(need(input$las_files, 'Please select a LAS file to plot'))

    # Plot the selected LAS file
    output$las_plot <- renderRglwidget({
      plot(las_files$data[[input$las_files]])
      rglwidget()
    })
  })

  observeEvent(input$plot_raster, {
    validate(need(input$raster_files, 'Please select a raster file to plot'))

    # Plot the selected raster file
    output$raster_plot <- renderPlot({
      terra::plot(raster_files$data[[input$raster_files]])
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

