#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shiny)

ui <- fluidPage(
    leafletOutput("map1")
)

map <- leaflet() %>% addCircleMarkers(
    lng = runif(10),
    lat = runif(10),
    layerId = paste0("marker", 1:10))
server <- function(input, output, session) {
    output$map1 <- renderLeaflet(map)
    
    observeEvent(input$map1_marker_click, {
        leafletProxy("map1", session) %>%
            removeMarker(input$map1_marker_click$id)
    })
}

app <- shinyApp(ui, server)
# }
# NOT RUN {
if (interactive()) app
# }
# NOT RUN {



