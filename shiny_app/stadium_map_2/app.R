library(tidyverse)
library(rvest)
library(tibble)
library(leaflet)
library(dplyr)
library(readr)
library(shinyWidgets)
# 
# load(here::here("dataset", "shiny_wins.RData"))
# load(here::here("dataset", "MLBstadiums.RData"))
# load(here::here("dataset", "win_perc.RData"))

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
    
      filter(win_perc, years == input$yearSelected)
    
  })
  
  output$map <- renderLeaflet({
    
    default <- win_perc %>% filter(year %in% "2002") #%>% trunc(win_perc)
    
    leaflet(MLBstadiums) %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>% 
      addTiles() %>% 
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        icon = icons, 
                        label = ~as.character(Venue), 
                        popup = ~as.character(default$win_perc)) %>% 
      addLegend(position = 'bottomleft', 
                colors = makeColorsandNames[,2],
                labels = makeColorsandNames[,1],
                opacity = 1,title = 'Divisions')
    
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
    proxy <- leafletProxy("map", data = filteredData)
    
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
