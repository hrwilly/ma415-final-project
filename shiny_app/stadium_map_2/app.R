# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# 
# --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(rvest)
library(tibble)
library(leaflet)
library(dplyr)
library(readr)


# ui <- ui <- fluidPage(
#   leafletOutput("mymap"),
#   p()
# )

ui <- fluidPage(
  fluidRow(column(width=12, leafletOutput("mymap"))),
  fluidRow(
    column(width=4, 
           selectizeInput(inputId = "yearSelected",label = "Year", choices = win_perc$year),
           uiOutput(win_perc$year))
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
    
    winning <- left_join( MLBstadiums, win_perc,by = c("Abbreviation" = "h_name")) %>% filter(year %in% uiOutput )
    
    
    m <- leaflet(winning) %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>% 
      addTiles() %>% 
      addAwesomeMarkers(~Longitude, ~Latitude, 
                        icon = icons, 
                        label = ~as.character(Venue), 
                        popup = ~as.character(winning)) %>% 
      addLegend(position = 'bottomleft', 
                colors = makeColorsandNames[,2],
                labels = makeColorsandNames[,1],
                opacity = 1,title = 'Divisions')
    m
  }) 
}


# # NOT WORKING CORRECTLY: Clicking city on map should update country and city selected 
# observe({
#   if(!is.null(input$map_marker_click)){
#     
#     updateSelectizeInput(
#       session, "yearSelected", 
#       selected = win_perc$year[(cities$lat==input$map_marker_click$lat)&(cities$long==input$map_marker_click$lng)])
#     
#    
#   
# }
# 
# })

shinyApp(ui, server, options = list(height = 550)) 
