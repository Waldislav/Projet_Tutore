library(shiny)
library(leaflet)
library(tidyverse)

# Assurez-vous que pfas_df est correctement préparé avant de lancer l'application

ui <- fluidPage(
  titlePanel("Évolution des substances par zone"),
  sidebarLayout(
    sidebarPanel(
      selectInput("substance", "Choisir une substance :", 
                  choices = unique(pfas_df$substance)),
      sliderInput("year", "Année :", 
                  min = min(pfas_df$year, na.rm = TRUE),
                  max = max(pfas_df$year, na.rm = TRUE),
                  value = min(pfas_df$year, na.rm = TRUE),
                  step = 1,
                  sep = "")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    pfas_df %>%
      filter(substance == input$substance, year == input$year, !is.na(lat), !is.na(lon), !is.na(final_value))
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    if(nrow(data) == 0){
      leaflet() %>%
        addTiles() %>%
        addPopups(lng = 2.2137, lat = 46.2276, popup = "Aucune donnée disponible pour ces critères.")
    } else {
      max_val <- max(data$final_value, na.rm = TRUE)
      
      leaflet(data, user) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon, ~lat,
          color = ~substance,
          popup = ~paste("Substance:", substance, "<br>",
                         "Valeur:", final_value, "<br>",
                         "Année:", year),
          radius = ~ifelse(max_val > 0, (final_value / max_val) * 10, 5),
          fillOpacity = 0.7,
          stroke = FALSE
        ) %>%
        addCircleMarkers(
          ~user$lon, ~user$lat,
          color = "#FF0000",
          popup = ~paste("Nom:", user$name, "<br>"),
          radius = ~ifelse(max_val > 0, (final_value / max_val) * 10, 5),
          fillOpacity = 0.7,
          stroke = FALSE
        ) %>%
        addLegend("bottomright", 
                  colors = unique(data$substance),
                  labels = unique(data$substance),
                  title = "Substance")
    }
  })
}

shinyApp(ui, server)