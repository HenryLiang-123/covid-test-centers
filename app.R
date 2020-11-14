library(shiny)
library(rvest)
library(tidyr)
library(dplyr)
library(leaflet)
library(DT)
library(ggmap)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  navbarPage("COVID-19 Test Centers LA County", id="main",
             tabPanel("Map", leafletOutput("map", height=1000)),
             tabPanel("Data", dataTableOutput("data")))
  
  
)

server <- function(input, output) {
  # Import data scraped from URL
  # Clean
  
  url <- read_html("https://corona-virus.la/testing-center-directory")
  names <- url %>% 
    html_nodes("li") %>%
    html_text() %>%
    unique()
  
  names <- names[8:length(names)]
  names <- names[1:390]
  names <- trimws(names)
  t <- tibble(names) %>%
    mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
    na.omit()
  t <- t[-(1:8), ]
  
  # Split names by center name and location
  # Create column for pop-up label (Center name, location)
  # Format Address to use geolocation
  center_location <- t %>%
    separate(names, c("center", "location"), sep = "[(]") %>%
    na.omit() 
  center_location$location <- trimws(unlist(strsplit(center_location$location, ")")))[nchar(trimws(unlist(strsplit(center_location$location, ")")))) > 1][-79]
  k <- center_location
  center_location <- center_location%>%
    mutate(pop_up = paste0('<strong>Name: </strong>', center,
                           '<br><strong>Address: </strong>', location)) %>%
    mutate(long = as.numeric(unlist(geocode(location, output = "latlona", source = "google")[1])))%>%
    mutate(lat = as.numeric(unlist(geocode(location, output = "latlona", source = "google")[2])))
  # Leaflet Map
  
  output$map <- renderLeaflet({
    leaflet(center_location) %>% 
      addCircles(lng = ~long, lat = ~lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = center_location, lat =  ~lat, lng = ~long, 
                       radius = 3, popup = ~as.character(pop_up), 
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  output$data <- renderDataTable(datatable(
    k, 
    colnames = c("", "Test Center Name", "Test Center Location")
  ))
 
}

shinyApp(ui = ui, server = server)