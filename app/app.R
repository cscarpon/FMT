# Source the required scripts 
source("app/global.R")
source("r/functions.R")
source("app/server.R")
source("r/pc_obj.R")


# Define the UI and server
ui <- source("app/ui.R", local = TRUE)$value
server <- source("app/server.R", local = TRUE)$value


# Run the app
shinyApp(ui = ui, server = server)
