# Source the required scripts from local directory

source("fmt/global.R")
source("r/functions.R")
source("fmt/server.R")
source("r/pc_obj.R")

# Define the UI and server
ui <- source("fmt/ui.R", local = TRUE)$value
server <- source("fmt/server.R", local = TRUE)$value

# Run the app
shinyApp(ui = ui, server = server)