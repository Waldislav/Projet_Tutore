library(sf)
library(leaflet)
library(shiny)

# Load France regions from GADM GeoPackage
#france_regions_sf <- st_read("../gadm41_FRA.gpkg", layer = "ADM_ADM_1")

france_regions_sf <- st_read("../gadm41_FRA.gpkg", layer = "ADM_ADM_1")
ui <- fluidPage(
  leafletOutput("map"),
  textOutput("region_name")
)
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(france_regions_sf) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~NAME_1,
        fillColor = "blue",
        color = "white",
        weight = 2,
        fillOpacity = 0.5
      )
  })
  
  output$region_name <- renderText({
    req(input$map_shape_click)
    input$map_shape_click$id
  })
}
shinyApp(ui, server)