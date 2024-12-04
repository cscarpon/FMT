# Run this script to start the FMT
source(file.path("./global.R"), local = TRUE)
source(file.path("./server.R"), local =  TRUE)
source(file.path("./r/functions.R"), local = TRUE)
source(file.path("./r/spatial_container.R"), local = TRUE)
source(file.path("./r/meta_obj.R"), local = TRUE)

# Define the UI and server
ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

# Run the app
<<<<<<< HEAD
shinyApp(ui = ui, server = server)


=======
shinyApp(ui = ui, server = server)
>>>>>>> 4300eb0fbd0b14f6922ae98b0aade55a26cdc6a6
