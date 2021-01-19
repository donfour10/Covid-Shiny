library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(sf)
library(shinythemes)

ui <- bootstrapPage(
    navbarPage("Covid Survey",
               theme = shinytheme("flatly"),
               tabPanel(
                   title = "Home",
                   h3("Introduction")
               ),
               tabPanel(
                   title = "World Map",
                   sidebarLayout(
                       sidebarPanel(
                           "Interactive Widgets"
                       ),
                       mainPanel(
                           "World Map"
                       )
                   )
               ),
               tabPanel(
                   title = "Correlation",
                   sidebarLayout(
                       sidebarPanel(
                           "Interactive Widgets"
                       ),
                       mainPanel(
                           "Correlation Scatter Plots"
                       )
                   )
               )
    )
)

server <- function(input, output) {


}

# Run the application
shinyApp(ui = ui, server = server)
