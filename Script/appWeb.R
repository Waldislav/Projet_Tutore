library(shiny)
library(leaflet)
library(tidyverse)

ui <- fluidPage(
  titlePanel("PFAS par Région"),
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
      filter(substance == input$substance, 
             year == input$year, 
             !is.na(lat), 
             !is.na(lon), 
             !is.na(final_value))
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~substance,
        popup = ~paste(
          "Région:", region, "<br>",
          "Substance:", substance, "<br>",
          "Valeur PFAS:", final_value, "<br>",
          "Année:", year
        )
      )
  })
}

shinyApp(ui, server)