library(shiny)
library(leaflet)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
        h1(strong("NFL Team Mentions on Twitter")),
        h4("Author: ",em("Brian Stroh")),
        h4("Date: ",em("December 11th, 2019")),
        br(),
        sidebarLayout(
            sidebarPanel( sliderInput("height",
                                      "Change Map Height:",
                                      min = 500,
                                      max = 1500,
                                      value = 800)
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Map",uiOutput("map")),
                    tabPanel("Analysis",
                             p("Let us explore the whether or not the proportion distributions for NFL team mentions are equal 
                               between all tweets and just tweets where users share their location."),
                             br(),
                             p("Null Hypothesis: The distribution of NFL team mentions for tweets where location data is available is identical to 
                               the distribution of NFL team mentions for tweets where location data is not available."),
                             p("Alternative Hypothesis: The distribution of NFL team mentions for tweets where location data is available is NOT identical to 
                               the distribution of NFL team mentions for tweets where location data is not available."),
                             br(),
                             br(),
                             plotOutput("plot"),
                             br(),
                             p("We can see from the distribution of proportions of team mentions that most teams have similar proportions of mentions
                               between tweets with and without locations shared. The largest exception is the Chargers, which have a much larger
                               proportion of tweets with location data than tweets without location data, relative to mentions of all other teams.
                               This will likely contribute to a rejected null hypothesis."),
                             br(),
                             textOutput("test"),
                             br(),
                             p("Conclusion:"),
                             p("At the 5% significance level, the null hypothesis is rejected. There is significant statistical evidence to prove that the proportion
                               of tweet mentions by NFL team are not identically distributed among tweets that share location data and tweets that do not share location."),
                             p("The results of this analysis and the map can be used to target ads for fanwear in locations not just near each team's stadium, but also in
                               other locations where each team's fans are. It seems that it will be relatively easy to target ads for Chargers fans and more difficult 
                               for Cardinals and Buccaneers fans, who do not share much location data.")),
                    tabPanel("Sources and Limitations",
                             p("The data in this application was mostly captured and cleaned in Python before being brought into R."),
                             p("The files used to create this data can be found here:"),
                             tags$a(href="https://github.com/brianjstroh/ds710fall2019finalproject", "Brian's GitHub Repo (Currently Private)"),
                             br(),
                             br(),
                             br(),
                             p("The data was collected using Twitter's API through the tweePy package in Python during the time frame of December 6th through December 10th."),
                             p("As tweet streams were collected only in this time frame, the statistical assumption of independence is violated."),
                             p("Tweets in the time frame were influenced by current events such as the latest cheating scandal of the Patriots spying on the Bengal's sidelines."),
                             p("Whereas one might expect the Patriots to create a lot of talk on Twitter between their stellar history, recent loss streak, power rankings, etc.,
                               one would not expect the Bengals (who have had an abysmal season so far and a fairly small fan base) to be in the top 10 most-talked-about team
                               on most weeks this season."),
                             p("Retweets were not filtered out, so that could be an additional factor impacting independence of sampled data."),
                             br(),
                             br(),
                             br(),
                             p("Additionally, no sentiment analysis was performed so tweet mentions were not analyzed for the tweeter's favorite team."),
                             p("The count of mentions simply implies that the teams are a popular topic for discussion, not that they're the fanbase's favorite teams."))
                            
                )
                
            )
        )
)

server <- function(input, output, session) {
    
#----------------------------------This section is for the Analysis Tab------------------------------------
    team_coords <- read.csv("team_coords.csv",stringsAsFactors = F)
    all_mentions <- read.csv("nfl_team_mentions.csv", stringsAsFactors = F, col.names = c("team", "not_placed"))
    
    placed_mentions <- team_coords %>% group_by(team = Teams) %>% summarise(placed = n())
    all_mentions <- all_mentions[order(all_mentions$team),]
    
    proportions <- data.frame(team = placed_mentions$team, 
                              not_placed = all_mentions$not_placed/sum(all_mentions$not_placed), 
                              placed = placed_mentions$placed/sum(placed_mentions$placed))
    
    for_graph <- melt(proportions, variable.name = "location", value.name = "proportions")
    
    output$plot <- renderPlot({ggplot(for_graph, aes(x = team, y = proportions, group = location, fill = location)) + 
        geom_col(position = position_dodge()) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
        labs(title = "Proportions of Team Mentions Among Tweets With and Without Location Shared")})
    
    
    output$test <- renderPrint({chisq.test(rbind(all_mentions$not_placed, placed_mentions$placed))})
#----------------------------------End of Analysis section----------------------------------
    
    
#--------------This section is for mapping the location distribution of NFL team mentions.----------------
    coords <- reactiveValues()
    coords$data <- data.table(read.csv("team_coords.csv"))
    
    mydf <- reactive({
        coords$data
    })
    
    teamIcons <- iconList(
        Ravens = makeIcon("Ravens.png", "Ravens.png", 20, 20),
        Jaguars = makeIcon("Jaguars.png", "Jaguars.png", 20, 20)
    )
    
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
            ) %>%
            setView(-100, 40, zoom = 4)

    })
}


# Run the application 
shinyApp(ui = ui, server = server)
