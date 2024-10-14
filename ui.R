# Define the UI
ui <- navbarPage(
  title = "Forest Monitoring Tool (FMT)",
  
  # First tab: Introduction
  tabPanel(
    title = "Introduction",
    fluidPage(
      h2("Welcome to the Forest Monitoring Tool (FMT)"),
      p("The FMT is designed to help users process, analyze, and visualize LiDAR data. 
       You can upload point cloud data in LAS or LAZ format, process Digital Terrain Models (DTMs), 
       Canopy Height Models (CHMs), and align point clouds for change detection. 
       Below are the steps to use the tool:"),
      tags$ul(
        tags$li("Step 1: Upload the source and target point clouds."),
        tags$li("Step 2: Run the desired functions, such as ICP Alignment or DTM/CHM generation."),
        tags$li("Step 3: View the results on the Leaflet map or in 3D plots."),
        tags$li("Step 4: Save the processed data and download it.")
      ),
      # Add the image
      tags$img(src = "C:\\Users\\cscar\\FMT\\www\\FMT.png", height = "400px", width = "100%", alt = "FMT Overview Image")
    )
  ),
  
  # Second tab: FMT UI
  tabPanel(
    title = "FMT Tool",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          fileInput("upload_file", "Upload Point Cloud (.laz or .las)", accept = c(".laz", ".las")),
          textInput("in_dir", "Input directory:", value = paste0(getwd(), "/data/")),
          textInput("out_dir", "Output directory:", value = paste0(getwd(), "/saves/")),
          numericInput("resolution", "Resolution:", value = 1),
          numericInput("crs", "CRS:", value = 32617),
          actionButton("confirm", "Confirm Inputs"),
          tags$hr(),
          h4("Data Processing"),
          fluidRow(
            column(6, selectInput("selected_source", "Select Source Point Cloud", choices = NULL)),
            column(6, selectInput("selected_target", "Select Target Point Cloud", choices = NULL)),
            column(6, selectInput("selected_buildings", "Select Your Building Footprints (.shp)", choices = NULL))
          ),
          fluidRow(
            column(12, actionButton("PC_confirm", "Confirm Point Cloud Selections", width = "100%"))
          ),
          fluidRow(
            column(6, actionButton("run_mask", "Generate Mask", width = "100%")),
            column(6, actionButton("run_denoise", "Remove Noise", width = "100%")),
            column(6, actionButton("run_icp", "ICP Alignment", width = "100%")),
            column(6, actionButton("dtm1", "Generate DTM for Source", width = "100%")),
            column(6, actionButton("dtm2", "Generate DTM for Target", width = "100%")),
            column(6, actionButton("chm1", "Generate CHM for Source", width = "100%")),
            column(6, actionButton("chm2", "Generate CHM for Target", width = "100%")),
          ),
          tags$hr(),
          h4("Post Processing"),
          fluidRow(
            column(6, selectInput("selected_processing", "Select which raster types to align", choices = c("","DTM", "CHM"))),
            column(6, actionButton("align_rasters", "Align Rasters", width = "100%", title = "Aligns Source to Target Raster")),
            column(6, actionButton("classify_raster", "Classify Rasters", width = "100%", title = "Difference and Classify Rasters"))
          ),
          tags$hr(),
          h4("Data Saving"),
          selectInput("io_obj", "Select PC to save", choices = NULL),
          fluidRow(
            column(6, actionButton("save_las", "Save LAS", width = "100%", title = "Save the current LAS object")),
            column(6, actionButton("save_dtm", "Save DTM", width = "100%", title = "Save the current DTM")),
            column(6, actionButton("save_chm", "Save CHM", width = "100%", title = "Save the current CHM")),
            column(6, actionButton("save_mask", "Save mask", width = "100%", title = "Save the current mask")),
            column(6, downloadButton("downloadData", "Save data", width = "100%", title = "Save all the data in the out directory"))
          ),
          tags$hr(),
          h4("Plotting"),
          fluidRow(
            column(6, actionButton("plot_source", "Plot Source Las", width = "100%", title = "Plot the Source Container")),
            column(6, actionButton("plot_target", "Plot Target Las", width = "100%", title = "Plot the Target Container")),
            column(6, actionButton("plot_leaf", "Plot to Leaflet", width = "100%", title = "Plot the current objects to a leaflet map")),
            column(6, actionButton("plot_results", "Plot Results", width = "100%", title = "Plot the difference results"))
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Directory Data", div(style = "height: 500px; overflow-y: auto;", tableOutput("plotmeta"))),
            tabPanel("Leaflet Map", leafletOutput("leafletmap", height = "500px")),
            tabPanel("3D Plot", rglwidgetOutput("plot3D", width = "100%", height = "500px")),
            tabPanel("2D Plot", plotOutput("plot2D"))
          ),
          tags$head(tags$style(HTML("
            #console_output {
              height: 500px;
              overflow-y: auto;
            }
          "))),
          tags$head(
            tags$style(HTML("
              .console-box {
                border: 1px solid #ccc;
                padding: 10px;
                margin-top: 20px;
                border-radius: 5px;
                background-color: #f9f9f9;
                height: 500px;
                overflow-y: auto;
              }
            "))
          ),
          uiOutput("console_output")
        )
      )
    )
  )
)
