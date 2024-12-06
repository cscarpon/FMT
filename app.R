# Run this script to start the FMT
source(file.path("./global.R"), local = TRUE)
source(file.path("./server.R"), local =  TRUE)
source(file.path("./r/functions.R"), local = TRUE)
source(file.path("./r/spatial_container.R"), local = TRUE)
source(file.path("./r/meta_obj.R"), local = TRUE)

# Define the UI and server
ui <- source(file.path("./ui.R"), local = TRUE)$value
server <- source(file.path("./server.R"), local = TRUE)$value

# Run the app
shiny::shinyApp(ui = ui, server = server)