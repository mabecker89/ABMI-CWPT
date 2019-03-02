#-------------------------------------------------------------------------------

# Shiny and shinydashboard learnings.

library(shiny)
library(shinydashboard)

# Three components to a dashboard: Header, Sidebar, Body

header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody()

# Combine all three

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {}

shinyApp(ui, server)

# Header


