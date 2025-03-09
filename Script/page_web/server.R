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
  
  # Graphique combiné existant
  output$combined_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    selected_substance <- input$substance
    
    # Filtrer les données par région sélectionnée pour le graphique existant
    pfas_data <- pfas %>%
      filter(region == region_id,
             substance %in% names(sort(table(substance), decreasing = TRUE))[1:9]) %>%
      select(region, substance, matrix, value) %>%
      mutate(value = as.numeric(value))
    
    # Créer le diagramme en barres empilées
    p1 <- ggplot(pfas_data, aes(x = matrix, y = value, fill = substance)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = "Quantité de PFAS détectée par milieu et par substance",
        x = "Milieu (Matrix)",
        y = "Quantité de PFAS détectée",
        fill = "Substance"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")
    
    # Appeler l'ancien graphique
    p2 <- create_combined_plot(region_id, selected_substance, input$show_prelevements, input$show_pfas_total, input$show_selected_pfas)
    
    # Afficher les deux graphiques
    grid.arrange(p1, p2, nrow = 2)
  })
  
  # Nouveau graphique en bougies : Nombre de lignes par matrix pour la région sélectionnée
  output$box_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    # Compter le nombre de lignes par région et par matrix dans le dataframe "france"
    france_count <- france %>%
      filter(region == region_id) %>%
      count(matrix)
    
    # Créer le graphique en barres avec des barres pleines
    ggplot(france_count, aes(x = matrix, y = n, fill = matrix)) +
      geom_bar(stat = "identity") +  # Barres pleines avec une couleur par matrix
      labs(
        title = paste("Nombre de lignes par milieu (Matrix) dans", region_id),
        x = "Milieu (Matrix)",
        y = "Nombre de lignes",
        fill = "Milieu"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")  # Palette de couleurs pour les barres
  })
  
  # Nouveau graphique pour la France
  output$france_plot <- renderPlot({
    # Agréger les données pour la France entière
    france_data <- resultats %>%
      group_by(year) %>%
      summarise(
        total_prelevements = n(),
        non_conforme_France = sum(non_conformes_France, na.rm = TRUE),
        non_conforme_Danemark = sum(non_conformes_Danemark, na.rm = TRUE),
        non_conforme_USA = sum(non_conformes_USA, na.rm = TRUE)
      ) %>%
      mutate(
        pourcentage_France = (non_conforme_France / total_prelevements) * 100,
        pourcentage_Danemark = (non_conforme_Danemark / total_prelevements) * 100,
        pourcentage_USA = (non_conforme_USA / total_prelevements) * 100
      )
    
    # Convertir les données en format long pour ggplot2
    france_long <- france_data %>%
      pivot_longer(
        cols = starts_with("pourcentage"),
        names_to = "reglementation",
        values_to = "pourcentage_non_conformes"
      ) %>%
      mutate(reglementation = case_when(
        reglementation == "pourcentage_France" ~ "France",
        reglementation == "pourcentage_Danemark" ~ "Danemark",
        reglementation == "pourcentage_USA" ~ "USA"
      ))
    
    # Créer le graphique pour la France
    ggplot(france_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Évolution du pourcentage de prélèvements non conformes en France",
        x = "Année",
        y = "Pourcentage de prélèvements non conformes",
        color = "Réglementation"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Nouveau graphique pour la région sélectionnée
  output$region_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    # Agréger les données pour la région sélectionnée
    region_data <- resultats_region_annee %>%
      filter(region == region_id) %>%
      group_by(year) %>%
      summarise(
        total_prelevements = n(),
        non_conforme_France = sum(non_conformes_France, na.rm = TRUE),
        non_conforme_Danemark = sum(non_conformes_Danemark, na.rm = TRUE),
        non_conforme_USA = sum(non_conformes_USA, na.rm = TRUE)
      ) %>%
      mutate(
        pourcentage_France = (non_conforme_France / total_prelevements) * 100,
        pourcentage_Danemark = (non_conforme_Danemark / total_prelevements) * 100,
        pourcentage_USA = (non_conforme_USA / total_prelevements) * 100
      )
    
    # Convertir les données en format long pour ggplot2
    region_long <- region_data %>%
      pivot_longer(
        cols = starts_with("pourcentage"),
        names_to = "reglementation",
        values_to = "pourcentage_non_conformes"
      ) %>%
      mutate(reglementation = case_when(
        reglementation == "pourcentage_France" ~ "France",
        reglementation == "pourcentage_Danemark" ~ "Danemark",
        reglementation == "pourcentage_USA" ~ "USA"
      ))
    
    # Créer le graphique pour la région sélectionnée
    ggplot(region_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Évolution du pourcentage de prélèvements non conformes dans", region_id),
        x = "Année",
        y = "Pourcentage de prélèvements non conformes",
        color = "Réglementation"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
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
