library(shiny)
library(future)

#Allow for parallel processing in the app.
plan(multisession)

# Source the required scripts
source("app/global.R")
source("r/functions.R")
source("app/server.R")


# Define the UI and server
ui <- source("app/ui.R", local = TRUE)$value
server <- source("app/server.R", local = TRUE)$value


# Run the app
shinyApp(ui = ui, server = server)
