#library(shiny)
# library(tidyverse)
# library(rvest)
# library(tibble)
# library(leaflet)
# library(dplyr)
# library(readr)

# load(here::here("dataset", "shiny_wins.RData"))
# load(here::here("dataset", "MLBstadiums.RData"))

ui <- fluidPage(
  fluidRow(column(width=12, leafletOutput("mymap"))),
  fluidRow(
    column(width=4, 
           selectInput(inputId = "yearSelected",
                            label = "Year", 
                          choices = win_perc$year, 
                         selected = "win_perc$year[2]"))
         
  )
)


server <- function(input, output, session){
  data <- reactive({
    x <- MLBstadiums
    
    filteredData=subset(shiny_wins, 'Year' == year)
    return(filteredData)
  })  
  
  output$mymap <- renderLeaflet({
    #df <- data()
    
    winning <- left_join( MLBstadiums, win_perc,by = c("Abbreviation" = "h_name")) %>% 
      filter(year %in% input$yearSelected)
    
    m <- leaflet(winning) %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>% 
      addTiles() %>% 
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        icon = icons, 
                        label = ~as.character(Venue), 
                        popup = ~as.character(winning$win_perc)) %>% 
      addLegend(position = 'bottomleft', 
                colors = makeColorsandNames[,2],
                labels = makeColorsandNames[,1],
                opacity = 1,title = 'Divisions')
    m
  }) 
}



shinyApp(ui, server, options = list(height = 550)) 
