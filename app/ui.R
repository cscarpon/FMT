library(shiny)

fluidPage(
    titlePanel("Tree Analyser 5000"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose first LAS file", accept = c(".las", ".laz", ".xyz")),
        fileInput("file2", "Choose second LAS file", accept = c(".las", ".laz", ".xyz")),
        numericInput("resolution", "Resolution:", value = 0.5),
        numericInput("crs", "CRS:", value = 4326),
        textInput("out_dir", "Output directory:", value = paste0(getwd(),"/data/" )),
        actionButton("confirm", "Confirm Inputs"),
        wellPanel(
          title = "Data Processing",
          selectInput("selected_obj", "Select Point Cloud Object to process and to plot", choices = NULL),
          selectInput("selected_obj2", "Select Point Cloud Object for comparison analysis", choices = NULL),
          actionButton("xyz", "Convert to XYZ", title = "Convert the input file to XYZ format"),
          actionButton("dtm1", "Generate DTM for PC1"),
          actionButton("dtm2", "Generate DTM for PC2"),
          actionButton("chm1", "Generate CHM for PC1"),
          actionButton("chm2", "Generate CHM for PC2"),
          actionButton("align_chms", "Align CHMs", title = "Aligns PC1 to PC2"),
          actionButton("classify_chm", "Classify CHM", title = "Difference and Classify CHMs"),
          actionButton("raster_statistics", "Calculate Statistics", title = "Calculate Raster Statistics")
        ),
        tags$hr(),
        wellPanel(
          title = "Data Saving",
          actionButton("save_las", "Save LAS", title = "Save the current LAS object"),
          actionButton("save_dtm", "Save DTM", title = "Save the current DTM"),
          actionButton("save_chm", "Save CHM", title = "Save the current CHM"),
          actionButton("save_mask", "Save mask", title = "Save the current mask")
        ),
        tags$hr(),
        wellPanel(
          title = "Plotting",
          actionButton("plot_leaf", "Plot to Leaflet", title = "Plot the current objects to a leaflet map"),
          actionButton("plot_las", "Plot LAS", title = "Plot the current LAS object"),
          actionButton("plot_results", "Plot Results", title = "Plot the difference results")
          # actionButton("plot_mask", "Plot Mask", title = "Plot the current Mask"),
          # actionButton("plot_chm", "Plot CHM", title = "Plot the current CHM"),
          # actionButton("plot_dtm", "Plot DTM", title = "Plot the current DTM"),
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Leaflet Map", leafletOutput("leafletmap")),
          tabPanel("3D Plot", rglwidgetOutput("plot3D")),
          tabPanel("2D Plot", plotOutput("plot2D")),
          )
      ))
)