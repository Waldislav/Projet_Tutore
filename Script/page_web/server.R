# Charger les fichiers de logique spécifique
source("page_web/map_logic.R")
source("page_web/plot_logic.R")

server <- function(input, output) {
  # Données filtrées pour la carte
  filtered_data <- reactive({
    data <- pfas
    
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
  
  # Carte Leaflet
  output$map <- renderLeaflet({
    create_map(filtered_data())  # Fonction définie dans map_logic.R
  })
  
  # Graphique combiné
  output$combined_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    selected_substance <- input$substance
    
    create_combined_plot(region_id, selected_substance, input$show_prelevements, input$show_pfas_total, input$show_selected_pfas)  # Fonction définie dans plot_logic.R
  })
  
  # Texte pour la région sélectionnée
  output$region_name <- renderText({
    req(input$map_shape_click, input$year)
    region_id <- input$map_shape_click$id
    selected_year <- input$year
    
    if (input$ignore_year) {
      region_data <- regions %>%
        filter(NAME_1 == region_id)
    } else {
      region_data <- regions_by_year %>%
        filter(NAME_1 == region_id, year == selected_year)
    }
    
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