# Run this script to start the FMT
source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

# Define the UI and server
ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

# Run the app
shinyApp(ui = ui, server = server)