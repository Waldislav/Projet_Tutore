library(shiny)
library(shinythemes)
library(leaflet)
library(scales)


ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("PFAS par Région"),
  sidebarLayout(
    sidebarPanel(
      selectInput("substance", "Choisir une substance :", 
                  choices = unique(pfas_group_filtre$substance)),
      sliderInput("year", "Année :", 
                  min = min(pfas$year, na.rm = TRUE),
                  max = max(pfas$year, na.rm = TRUE),
                  value = min(pfas$year, na.rm = TRUE),
                  step = 1,
                  sep = ""),
      checkboxInput("ignore_year", "Ignorer l'année", value = FALSE),  # Ajout de la checkbox
      uiOutput("region_name")
    ),
    mainPanel(
      leafletOutput("map")
    )
  ),
)


server <- function(input, output) {
  filtered_data <- reactive({
    data <- pfas
    
    # Si "ignore_year" est coché, on ne filtre pas par année
    if (!input$ignore_year) {
      data <- data %>%
        filter(year == input$year)
    }
    
    data %>%
      filter(substance == input$substance, 
             !is.na(lat), 
             !is.na(lon), 
             !is.na(value))
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        data = regions,
        layerId = ~NAME_1,  # Identifier les polygones par leur nom
        fillColor = "transparent", # Couleur initiale (transparent)
        color = "white",    # Couleur de la bordure
        weight = 2,         # Épaisseur de la bordure
        fillOpacity = 0.0,  # Opacité initiale
        highlightOptions = highlightOptions(
          weight = 3,       # Épaissir la bordure au survol
          color = "blue",   # Bordure bleue au survol
          fillColor = "blue", # Remplissage bleu au survol
          fillOpacity = 0.2  # Opacité plus élevée au survol # Mettre la forme au premier plan
        )
      ) %>%
      addCircleMarkers(
        data = filtered_data(),
        ~lon, ~lat,
        color = "black",
        opacity = 1,
        weight = 1,
        fillColor = "orange",
        fillOpacity = 1,
        radius = 3,
        popup = ~paste(
          "Région:", region, "<br>",
          "Ville:", city, "<br>",
          "Substance:", substance, "<br>",
          "Valeur PFAS:", value, "<br>",
          "Année:", year
        )
      ) %>%
      addCircleMarkers(
        data = user,
        ~lon, ~lat,
        color = "grey",
        radius = 3,
        fillColor = "grey",
        fillOpacity = 1,
        popup = ~paste("Utilisateur : ", name)
      ) %>%
      addCircleMarkers(
        data = producteur,
        ~lon, ~lat,
        color = "black",
        radius = 3,
        fillColor = "black",
        fillOpacity = 1,
        popup = ~paste("Producteur : ", name)
      )
  })
  
  observeEvent(input$map_zoom, {
    zoom_level <- input$map_zoom
    radius_scale <- pmax(5, 10 / zoom_level) 
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered_data(),
        ~lon, ~lat,
        color = "black",
        opacity = 1,
        weight = 1,
        fillColor = "orange",
        fillOpacity = 1,
        radius = radius_scale,
        popup = ~paste(
          "Région:", region, "<br>",
          "Ville:", city, "<br>",
          "Substance:", substance, "<br>",
          "Valeur PFAS:", value, "<br>",
          "Année:", year
        )
      ) %>%
      addCircleMarkers(
        data = user,
        ~lon, ~lat,
        color = "grey",
        radius = radius_scale-1,
        fillColor = "grey",
        fillOpacity = 1,
        popup = ~paste("Utilisateur : ", name)
      ) %>%
      addCircleMarkers(
        data = producteur,
        ~lon, ~lat,
        color = "black",
        radius = radius_scale-1,
        fillColor = "black",
        fillOpacity = 1,
        popup = ~paste("Producteur : ", name)
      )
  })
  
  output$region_name <- renderText({
    req(input$map_shape_click, input$year)
    region_id <- input$map_shape_click$id
    selected_year <- input$year
    
    # Si "ignore_year" est coché, ne pas filtrer par année
    if (input$ignore_year) {
      region_data <- regions %>%
        filter(NAME_1 == region_id)  # Filtrer seulement par région
    } else {
      region_data <- regions_by_year %>%
        filter(NAME_1 == region_id, year == selected_year)  # Filtrer par région et année
    }
    
    # Construire le texte à afficher
    if (nrow(region_data) == 0) {
      paste(
        "<b>Région sélectionnée :</b>", region_id, "<br>",
        "<b>Aucune donnée disponible pour l'année :</b>", selected_year
      )
    } else {
      paste(
        "<b>Région sélectionnée :</b>", region_data$NAME_1, "<br>",
        "<b>Nombre de PFAS :</b>", region_data$nb_pfas, "<br>",
        "<b>Somme des valeurs des PFAS :</b> ", region_data$sum_pfas, "<br>",
        "<b>Nombre de producteurs :</b> ", region_data$nb_producteur, "<br>",
        "<b>Nombre d'utilisateurs :</b> ", region_data$nb_utilisateurs, "<br>"
      )
    }
  })
}

shinyApp(ui, server)