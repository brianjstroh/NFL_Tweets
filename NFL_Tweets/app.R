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
library(sp)

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
    
    teamIcons <- iconList(
        Ravens = makeIcon("Ravens.png", "Ravens.png", 20, 20),
        Jaguars = makeIcon("Jaguars.png", "Jaguars.png", 20, 20)
    )
    
    #leafIcons <- icons(
    #    iconUrl = case_when(select(mydf(),Teams) =="Ravens" ~ "Ravens.png",
    #                        TRUE ~ "Jaguars.png"
    #    ),
    #    iconWidth = 38, iconHeight = 95,
    #    iconAnchorX = 22, iconAnchorY = 94
    #)
    output$map = renderUI({
        leafletOutput('nfl_map', height = input$height)
    })
    
    
    output$nfl_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Cardinals'),
                Latitude,Longitude), 
                color = '#99213E',
                stroke = F,
                group = 'Cardinals') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Falcons'),
                Latitude,Longitude), 
                color = '#A9162D',
                stroke = F,
                group = 'Falcons') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Ravens'),
                Latitude,Longitude),
                color = '#241075',
                stroke = F,
                group = 'Ravens')%>%#, clusterOptions = markerClusterOptions()) %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Bills'),
                Latitude,Longitude), 
                color = '#00308F',
                stroke = F,
                group = 'Bills') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Panthers'),
                Latitude,Longitude), 
                color = '#0084CD',
                stroke = F,
                group = 'Panthers') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Bears'),
                Latitude,Longitude), 
                color = '#CA3700',
                stroke = F,
                group = 'Bears') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Bengals'),
                Latitude,Longitude), 
                color = '#FE4E00',
                stroke = F,
                group = 'Bengals') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Browns'),
                Latitude,Longitude), 
                color = '#311D00',
                stroke = F,
                group = 'Browns') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Cowboys'),
                Latitude,Longitude), 
                color = '#FFFFFF',
                stroke = F,
                group = 'Cowboys') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Broncos'),
                Latitude,Longitude), 
                color = '#002145',
                stroke = F,
                group = 'Broncos') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Lions'),
                Latitude,Longitude), 
                color = '#B0B7BC',
                stroke = F,
                group = 'Lions') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Packers'),
                Latitude,Longitude), 
                color = '#1F3731',
                stroke = F,
                group = 'Packers') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Texans'),
                Latitude,Longitude), 
                color = '#022030',
                stroke = F,
                group = 'Texans') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Colts'),
                Latitude,Longitude), 
                color = '#002B61',
                stroke = F,
                group = 'Colts') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Jaguars'),
                Latitude,Longitude), 
                color = '#006779',
                stroke = F,
                group = 'Jaguars')%>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Chiefs'),
                Latitude,Longitude), 
                color = '#E60F31',
                stroke = F,
                group = 'Chiefs') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Chargers'),
                Latitude,Longitude), 
                color = '#F0AE00',
                stroke = F,
                group = 'Chargers') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Rams'),
                Latitude,Longitude), 
                color = '#002145',
                stroke = F,
                group = 'Rams') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Dolphins'),
                Latitude,Longitude), 
                color = '#008E98',
                stroke = F,
                group = 'Dolphins') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Vikings'),
                Latitude,Longitude), 
                color = '#4F2185',
                stroke = F,
                group = 'Vikings') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Patriots'),
                Latitude,Longitude), 
                color = '#C8032B',
                stroke = F,
                group = 'Patriots') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Saints'),
                Latitude,Longitude), 
                color = '#D4BD8A',
                stroke = F,
                group = 'Saints') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Giants'),
                Latitude,Longitude), 
                color = '#0B2265',
                stroke = F,
                group = 'Giants') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Jets'),
                Latitude,Longitude), 
                color = '#1F3731',
                stroke = F,
                group = 'Jets') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Raiders'),
                Latitude,Longitude), 
                color = '#000000',
                stroke = F,
                group = 'Raiders') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Eagles'),
                Latitude,Longitude), 
                color = '#004C55',
                stroke = F,
                group = 'Eagles') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Steelers'),
                Latitude,Longitude), 
                color = '#F0AE00',
                stroke = F,
                group = 'Steelers') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == '49ers'),
                Latitude,Longitude), 
                color = '#AA0000',
                stroke = F,
                group = '49ers') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Seahawks'),
                Latitude,Longitude), 
                color = '#69BE28',
                stroke = F,
                group = 'Seahawks') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Buccaneers'),
                Latitude,Longitude), 
                color = '#D50A0A',
                stroke = F,
                group = 'Buccaneers') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Titans'),
                Latitude,Longitude), 
                color = '#4B92DB',
                stroke = F,
                group = 'Titans') %>%
            addCircleMarkers(data = select(
                filter(mydf(), Teams == 'Redskins'),
                Latitude,Longitude), 
                color = '#773141',
                stroke = F,
                group = 'Redskins') %>%
            
            #Add filtering for teams
            addLayersControl(
                baseGroups = c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns',
                                  'Cowboys','Broncos','Lions','Packers','Texans','Colts','Jaguars','Chiefs','Chargers',
                                  'Rams','Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles',
                                  'Steelers','49ers','Seahawks','Buccaneers','Titans','Redskins'),
                options = layersControlOptions(collapsed = F)
            )

    })
    
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
