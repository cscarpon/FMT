library(shiny)

fluidPage(
    titlePanel("Forest Monitoring Tool (FMT)"),
    sidebarLayout(
      sidebarPanel(
        textInput("in_dir", "Input directory:", value = paste0(getwd(),"/data/"),
        numericInput("resolution", "Resolution:", value = 1),
        numericInput("crs", "CRS:", value = 4326),
        textInput("out_dir", "Output directory:", value = paste0(getwd(),"/data/" )),
        actionButton("confirm", "Confirm Inputs"),
        wellPanel(
          title = "Data Processing",
          selectInput("selected_obj", "Select Point Cloud Object to process and to plot", choices = NULL),
          selectInput("selected_obj2", "Select Point Cloud Object for comparison analysis", choices = NULL),
          actionButton("dtm1", "Generate DTM for PC1"),
          actionButton("dtm2", "Generate DTM for PC2"),
          actionButton("chm1", "Generate CHM for PC1"),
          actionButton("chm2", "Generate CHM for PC2"),
          actionButton("align_chms", "Align CHMs", title = "Aligns PC1 to PC2"),
          actionButton("classify_chm", "Classify CHM", title = "Difference and Classify CHMs"),
        ),
        tags$hr(),
        wellPanel(
          title = "Data Saving",
          selectInput("io_obj", "Select PC to save", choices = NULL),
          actionButton("save_las", "Save LAS", title = "Save the current LAS object"),
          actionButton("save_dtm", "Save DTM", title = "Save the current DTM"),
          actionButton("save_chm", "Save CHM", title = "Save the current CHM"),
          actionButton("save_mask", "Save mask", title = "Save the current mask"),
          actionButton("save_pc", "Save PC", title = "Save Point Cloud 1"),
        ),
        tags$hr(),
        wellPanel(
          title = "Plotting",
          selectInput("plotting_obj", "Select PC to Plot", choices = NULL),
          actionButton("plot_leaf", "Plot to Leaflet", title = "Plot the current objects to a leaflet map"),
          actionButton("plot_las", "Plot LAS", title = "Plot the current LAS object"),
          actionButton("plot_results", "Plot Results", title = "Plot the difference results")
        ) 
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Directory Data", plotOutput("plotmeta")),
          tabPanel("Leaflet Map", leafletOutput("leafletmap")),
          tabPanel("3D Plot", rglwidgetOutput("plot3D")),
          tabPanel("2D Plot", plotOutput("plot2D"))
          ),
          tags$head(tags$style(HTML("
                              #console_output {
                                height: 300px;
                                overflow-y: auto;
                              }
                            "))),
        verbatimTextOutput("console_output")
      )
    )
 )
)