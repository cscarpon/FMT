library(shiny)

fluidPage(
    titlePanel("Forest Monitoring Tool (FMT)"),
    sidebarLayout(
      sidebarPanel(
        textInput("in_dir", "Input directory:", value = paste0(getwd(),"/data/")),
        textInput("out_dir", "Output directory:", value = paste0(getwd(),"/data/" )),
        numericInput("resolution", "Resolution:", value = 1),
        numericInput("crs", "CRS:", value = 32617),
        actionButton("confirm", "Confirm Inputs"),
        wellPanel(
          title = "Data Processing",
          selectInput("selected_source", "Select Source Point Cloud", choices = NULL),
          selectInput("selected_target", "Select Target Point Cloud", choices = NULL),
          actionButton("PC_confirm", "Confirm Point Cloud Selections"),
          actionButton("run_icp", "ICP Alignment"),
          actionButton("dtm1", "Generate DTM for Source"),
          actionButton("dtm2", "Generate DTM for Target"),
          actionButton("chm1", "Generate CHM for Source"),
          actionButton("chm2", "Generate CHM for Target"),
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
        ),
        tags$hr(),
        wellPanel(
          title = "Plotting",
          actionButton("plot_source", "Plot Source Las", title = "Plot the Source Container"),
          actionButton("plot_target", "Plot Target Las", title = "Plot the Target Container"),
          actionButton("plot_leaf", "Plot to Leaflet", title = "Plot the current objects to a leaflet map"),
          actionButton("plot_results", "Plot Results", title = "Plot the difference results")
        ) 
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Directory Data", tableOutput("plotmeta")),
          tabPanel("Leaflet Map", leafletOutput("leafletmap")),
          tabPanel("3D Plot", rglwidgetOutput("plot3D")),
          tabPanel("2D Plot", plotOutput("plot2D"))
          ),
          tags$head(tags$style(HTML("
                              #console_output {
                                height: 500px;
                                overflow-y: auto;
                              }
                            "))),
        verbatimTextOutput("console_output")
      )
    )
  )
