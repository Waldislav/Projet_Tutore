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
        ~lon, ~lat,
        color = ~substance,
        fillColor = "red",
        opacity = ~rescale(value, to = c(0.1, 1)),  # Ajuster l'opacité en fonction de final_value
        popup = ~paste(
          "Région:", region, "<br>",
          "Ville:", city, "<br>",
          "Substance:", substance, "<br>",
          "Valeur PFAS:", value, "<br>",
          "Année:", year
        )
      ) %>%
      addCircleMarkers(
        data = user,  # Remplacer par vos données utilisateur
        ~lon, ~lat,    # Latitude et longitude des utilisateurs
        color = "grey",  # Choisir une couleur pour les utilisateurs
        radius = 5,  # Taille des marqueurs
        fillColor = "grey",  # Couleur de remplissage
        fillOpacity = 1,   # Opacité des marqueurs
        popup = ~paste("Utilisateur : ", name)  # Popup avec le nom de l'utilisateur
      ) %>%
      # Ajouter les points des producteurs
      addCircleMarkers(
        data = producteur,  # Remplacer par vos données producteur
        ~lon, ~lat,    # Latitude et longitude des producteurs
        color = "black",  # Choisir une couleur pour les producteurs
        radius = 5,  # Taille des marqueurs
        fillColor = "black",  # Couleur de remplissage
        fillOpacity = 1,   # Opacité des marqueurs
        popup = ~paste("Producteur : ", name)  # Popup avec le nom du producteur
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