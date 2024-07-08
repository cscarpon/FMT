ui <- fluidPage(
  titlePanel("Forest Monitoring Tool (FMT)"),
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
        column(6, selectInput("selected_target", "Select Target Point Cloud", choices = NULL))
      ),
      fluidRow(
        column(12, actionButton("PC_confirm", "Confirm Point Cloud Selections", width = "100%"))
      ),
      fluidRow(
        column(6, actionButton("run_icp", "ICP Alignment", width = "100%")),
        column(6, actionButton("dtm1", "Generate DTM for Source", width = "100%")),
        column(6, actionButton("dtm2", "Generate DTM for Target", width = "100%")),
        column(6, actionButton("chm1", "Generate CHM for Source", width = "100%")),
        column(6, actionButton("chm2", "Generate CHM for Target", width = "100%")),
        column(6, actionButton("align_chms", "Align CHMs", width = "100%", title = "Aligns PC1 to PC2")),
        column(6, actionButton("classify_chm", "Classify CHM", width = "100%", title = "Difference and Classify CHMs"))
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