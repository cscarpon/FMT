library(shiny)

fluidPage(
    titlePanel("Tree Analyser 5000"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose LAS or XYZ file",
                  accept = c(".las", ".laz", ".xyz")),
        textInput("directory", "Enter your directory path here"),
        wellPanel(
          title = "Data Processing",
          actionButton("mask", "Get Mask", title = "Get the mask of the input file"),
          actionButton("xyz", "Convert to XYZ", title = "Convert the input file to XYZ format"),
          actionButton("dtm", "Generate DTM", title = "Generate DTM from the input file"),
          actionButton("chm", "Generate CHM", title = "Generate CHM from the input file")
        ),
        tags$hr(),
        wellPanel(
          title = "File Saving",
          actionButton("submit_directory", "Submit Directory"),
          actionButton("save_las", "Save LAS", title = "Save the current LAS object"),
          actionButton("save_dtm", "Save DTM", title = "Save the current DTM"),
          actionButton("save_chm", "Save CHM", title = "Save the current CHM"),
          downloadButton("downloadXYZ", "Download XYZ"),
          downloadButton("downloadDTM", "Download DTM"),
          downloadButton("downloadCHM", "Download CHM")
        ),
        tags$hr(),
        wellPanel(
          title = "Plotting",
          actionButton("plot_las", "Plot LAS", title = "Plot the current LAS object"),
          actionButton("plot_chm", "Plot CHM", title = "Plot the current CHM"),
          actionButton("plot_dtm", "Plot DTM", title = "Plot the current DTM"),
          selectInput("selected_obj", "Select Point Cloud Object to Plot", choices = NULL)
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("3D Plot", rglwidgetOutput("plot3D")),
          tabPanel("2D Plot", plotOutput("plot2D")),
          tabPanel("Object Status", tableOutput("status_table"))
          )
      ))
)