library(tidyverse)
library(rvest)
library(tibble)
library(leaflet)
library(dplyr)
library(readr)
library(shinyWidgets)
library(readr)

# load("shiny_wins.RData")
# load("MLBstadiums.RData")
# load("win_perc.RData")

MLBstadiums <- read_csv("shiny_app/stadium_map_2/MLBstadiums.csv")
shiny_wins <- read_csv("shiny_app/stadium_map_2/shiny_wins.csv")
win_perc <- read_csv("shiny_app/stadium_map_2/win_perc.csv")

win_perc <- win_perc %>% mutate(win_stats = win_perc) %>%
   mutate_if(is.numeric, ~round(., 1))
  
  
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'CHC'] <- 'CHN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'CHW'] <- 'CHA'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'KCR'] <- 'KCN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'LAA'] <- 'ANA'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'LAD'] <- 'LAN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'MIA'] <- 'FLO'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'NYM'] <- 'NYN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'NYY'] <- 'NYA'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'SDP'] <- 'SDN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'SFG'] <- 'SFN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'STL'] <- 'SLN'
MLBstadiums$Abbreviation[MLBstadiums$Abbreviation == 'TBR'] <- 'TBA'

makeColorsandNames <- data.frame(divisions = c('AL East','AL Central','AL West','NL East','NL Central','NL West'), 
                                 division.cent = c('#CC0000','#3399FF','#FFA500','#9ACD32','#483D8B','#000000'))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                pickerInput("yearSelected", 
                            label = "Select a Year:",
                          choices = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011", "2012", "2013", "2014", "2015", "2016")),
                
                checkboxInput("legend", "Show legend", TRUE)
    )
  )



server <- function(input, output, session){
  
  filteredData <- reactive({
    
      filter(win_perc, year == input$yearSelected) %>% 
      left_join(., MLBstadiums, by=c("h_name" = "Abbreviation")) 
      
    
  })
  
  output$map <- renderLeaflet({
    
    leaflet(filteredData()) %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>% 
      addTiles() %>% 
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        icon = icons, 
                        label = ~as.character(Venue), 
                        popup=paste("Winning Percentage:", filteredData$win_stats, "<br>",
                                    "Pitcher:",  "", "<br>", 
                                    "Team Salary:", "")) %>% 
      addLegend(position = 'bottomleft', 
                colors = makeColorsandNames[,2],
                labels = makeColorsandNames[,1],
                opacity = 1,
                title = 'Divisions')
    
  })
  
   # observe({
   #  leafletProxy("map", data = filteredData()) %>%
   #   addAwesomeMarkers(~Longitude, ~Latitude,
   #                      icon = icons,
   #                     label = ~as.character(Venue),
   #                     popup = ~as.character(percentage))
   # 
   # 
   # })
  
  # observe({
  #   proxy <- leafletProxy("map", data = filteredData)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   
  #   proxy %>%  addMarkers(~Longitude, ~Latitude, 
  #                          icon = icons, 
  #                          popup = ~as.character(percentage))
  # 
  #   
  # })

  
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>%  addLegend(position = 'bottomleft', 
                           colors = makeColorsandNames[,2],
                           labels = makeColorsandNames[,1],
                           opacity = 1,title = 'Divisions')
    }
  })

  
}



shinyApp(ui, server, options = list(height = 550)) 
