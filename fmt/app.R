# Run this script to start the FMT

source("fmt/global.R")
source("fmt/server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

# Define the UI and server
ui <- source("fmt/ui.R", local = TRUE)$value
server <- source("fmt/server.R", local = TRUE)$value

# Run the app
shinyApp(ui = ui, server = server)
