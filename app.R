# Run packages
library(shiny)
library(rsconnect)

# Source other parts of app.
source("ui.R")
source("server.R")

# Run shiny app
shinyApp(ui = ui, server = server)
