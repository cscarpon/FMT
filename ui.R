# Define the UI
ui <- navbarPage(
  title = "CloudFlux (CF)",
  
  # First tab: Introduction
  tabPanel(
    title = "Introduction",
    fluidPage(
      h2("Welcome to CloudFlux (CF)"),
      tags$div(
        style = "text-align: justify;", # Ensures the text fills the container and is justified
        "CF is designed to visualize, process, and analyze point cloud data. It can ingest LAS or LAZ formats. CF will create Digital Terrain Models (DTMs) and normalized Digital Surface Models (nDSM) and align point clouds for change detection. User data uploaded to CF is not saved and does not persist in the application. CF is free for use and it was built on the efforts of the open-source and open-access communities.", tags$br(),
        tags$br(),
        tags$b("Disclaimer:"),"CF is an educational tool and is not intended to replace professional advice or certified data processing workflows.The outputs are provided 'as is,' with no guarantee of accuracy, completeness, or suitability for any specific purpose. The developers of CF are not liable for any errors, inaccuracies, or decisions made based on its use. Use at your own risk.", tags$br(),
        tags$br(),
<<<<<<< HEAD
        "Users can uploaded their own point clouds, or they can use the data for Sunnybrook Campus (SB_19.laz and SB_23.laz) which comes preloaded. Below are the steps to use the tool:", tags$br(),
        tags$br()
      ),
      tags$ul(
        tags$li("Step 1: Upload the source and target point clouds. Confirm Inputs"),
        tags$li("Step 2: Select your source and target point clouds. Confirm Point Clouds "),
        tags$li("Step 3: Create Mask for Point Clouds"),
        tags$li("Step 4: Denoise point clouds."),
        tags$li("Step 5: ICP Alignment (currently not working for web tool)."),
        tags$li("Step 6: Generate DTMs for source and target."),
        tags$li("Step 7: Generate nDSMs for source and target"),
        tags$li("Step 8: Select DTM or nDSM for change detection"),
        tags$li("Step 9: Align rasters."),
        tags$li("Step 10: Classify"),
        tags$li("Step 11: Visualize the outputs in both maps (Plot to Leaflet), 2D (plot statistics), and 3D plots (Plot Las)."),
=======
        "Below are the steps to use the tool:", tags$br(),
        tags$br()
      ),
      tags$ul(
        tags$li("Step 1: Upload the source and target point clouds."),
        tags$li("Step 2: Denoise point clouds."),
        tags$li("Step 3: Create Mask for Point Clouds"),
        tags$li("Step 4: Conduct an ICP alignment between the source and target point cloud."),
        tags$li("Step 5: Generate DTMs with the provided resolution."),
        tags$li("Step 6: Align source and target rasters for change detection."),
        tags$li("Step 7: Process change detection between either nDSMs or DTMs."),
        tags$li("Step 8: Generate raster statistics from the change detection layer."),
        tags$li("Step 9: Visualize the outputs in both maps, 2D, and 3D plots."),
>>>>>>> 4300eb0fbd0b14f6922ae98b0aade55a26cdc6a6
      ),
      tags$style(HTML(".responsive-img {max-width: 100%;height: auto;}")),
      # Add the image
      div(class = "responsive-img",
          imageOutput("photo")
        )
      )
    ),
  # Second tab: CF UI
  tabPanel(
    title = "CloudFlux",
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
            column(12, actionButton("PC_confirm", "Confirm Point Cloud Selections",height = "auto", width = "100%"))
          ),
          fluidRow(
            column(6, actionButton("run_mask", "Generate Mask", width = "100%")),
            column(6, actionButton("run_denoise", "Remove Noise", width = "100%")),
            column(6, actionButton("run_icp", "ICP Alignment", width = "100%")),
            column(6, actionButton("dtm1", "Generate DTM for Source", width = "100%")),
            column(6, actionButton("dtm2", "Generate DTM for Target", width = "100%")),
            column(6, actionButton("chm1", "Generate nDSM for Source", width = "100%")),
            column(6, actionButton("chm2", "Generate nDSM for Target", width = "100%")),
          ),
          tags$hr(),
          h4("Post Processing"),
          fluidRow(
            column(6, selectInput("selected_processing", "Select which raster types to align", choices = c("","DTM", "nDSM"))),
            column(6, actionButton("align_rasters", "Align Rasters", width = "100%", title = "Aligns Source to Target Raster")),
            column(6, actionButton("classify_raster", "Classify Rasters", width = "100%", title = "Difference and Classify Rasters"))
          ),
          tags$hr(),
          h4("Data Saving"),
          selectInput("io_obj", "Select PC to save", choices = NULL),
          fluidRow(
            column(6, actionButton("save_las", "Save LAS", width = "100%", title = "Save the current LAS object")),
            column(6, actionButton("save_dtm", "Save DTM", width = "100%", title = "Save the current DTM")),
            column(6, actionButton("save_chm", "Save nDSM", width = "100%", title = "Save the current nDSM")),
            column(6, actionButton("save_mask", "Save mask", width = "100%", title = "Save the current mask")),
            column(6, actionButton("save_SC", "Save Spatial Containers", width = "100%", title = "The modified spatial containers")),
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
