library(shiny)
library(tidyverse)
library(rvest)
library(tibble)
library(leaflet)
library(dplyr)
library(readr)
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
                selectInput(inputId = "yearSelected",
                              label = "Year",
                              choices = win_perc$year,
                              selected = "win_perc$year[2]"),
                checkboxInput("legend", "Show legend", TRUE)
    )
  )



server <- function(input, output, session){
  filteredData <- reactive({
    
    left_join(MLBstadiums, win_perc, by = c("Abbreviation" = "h_name")) %>% 
      filter(year %in% "input$yearSelected", Sport == "MLB") %>%
      mutate(percentage = win_perc)
    
    
  })  
  
  output$map <- renderLeaflet({
    
    
    leaflet(MLBstadiums) %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>% 
      addTiles() %>% 
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        icon = icons, 
                        label = ~as.character(Venue)) %>% 
      addLegend(position = 'bottomleft', 
                colors = makeColorsandNames[,2],
                labels = makeColorsandNames[,1],
                opacity = 1,title = 'Divisions')
    
  })
  
  # observe({
  #   leafletProxy("map", data = filteredData()) %>%
  #     addAwesomeMarkers(~Longitude, ~Latitude,
  #                       icon = icons, 
  #                       label = ~as.character(Venue),
  #                       popup = ~as.character(percentage))
  
  
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
