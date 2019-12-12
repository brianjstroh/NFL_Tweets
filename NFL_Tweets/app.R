#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(data.table)

ui <- fluidPage(
        sidebarLayout(
            sidebarPanel( sliderInput("height",
                                      "Change Map Height:",
                                      min = 500,
                                      max = 1500,
                                      value = 1000)
            ),
            mainPanel(
                uiOutput("map")
            )
        )
)

server <- function(input, output, session) {
    coords <- reactiveValues()
    coords$data <- data.table(read.csv("team_coords.csv"))

    mydf <- reactive({
        coords$data
    })
    
    output$map = renderUI({
        leafletOutput('nfl_map', height = input$height)
    })
    
    output$nfl_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = select(
                    filter(mydf(), Teams == 'Ravens'),
                    Latitude,Longitude),
                group = 'Ravens')%>%#, clusterOptions = markerClusterOptions()) %>%
            addCircleMarkers(data = select(
                    filter(mydf(), Teams == 'Patriots'),
                    Latitude,Longitude),
                    group = 'Patriots')%>%#, clusterOptions = markerClusterOptions()) %>%
            
            #Add filtering for teams
            addLayersControl(
                overlayGroups = c('Ravens', 'Patriots'),
                options = layersControlOptions(collapsed = F)
            )

    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
