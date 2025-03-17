# Charger les fichiers de logique spécifique
source("page_web/map_logic.R")
source("page_web/plot_logic.R")

generate_pfas_table <- function(rowid) {
  pfas_filtered <- pfas %>% filter(rowid == !!rowid)  # Sélectionner les pfas correspondant au rowid
  
  if (nrow(pfas_filtered) == 0) {
    return("Aucune donnée PFAS")
  }
  
  table_html <- paste0(
    "<table border='1' style='border-collapse: collapse; width: 100%;'>",
    "<tr><th>Substance</th><th>Valeur</th></tr>",
    paste0("<tr><td>", pfas_filtered$substance, "</td><td>", pfas_filtered$value,pfas_filtered$unit, "</td></tr>", collapse = ""),
    "</table>"
  )
  
  return(table_html)
}


server <- function(input, output, session) {
  # Données filtrées pour la carte
  filtered_data <- reactive({
    data <- pfas
    
    if (!input$ignore_year) {
      data <- data %>%
        filter(year == input$year)
    }
    
    data <- data%>%
      filter(!is.na(lat),
             !is.na(lon),
             !is.na(value))
    
    if(input$substance != "Tous") {
      data <- data %>%
        filter(substance == input$substance)
    }
    
    if(input$matrix != "Tous") {
      data <- data %>%
        filter(matrix == input$matrix)
    }
    
    data_f <- france_norme %>% filter(rowid %in% data$rowid)
    
    return(data_f)
    
    
    #data <- data %>%
    #  rowwise() %>%
    #  mutate(pfas_table = generate_pfas_table(rowid))
  })
  
  # Carte Leaflet
  output$map <- renderLeaflet({
    create_map(input,filtered_data(), input$regle)  # Fonction définie dans map_logic.R
  })
  
  output$cam_pfas_plot <- renderPlot({
    ggplot(pfas_group_filtre, aes(x = "", y = total_value, fill = substance)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = ""), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(
        values = RColorBrewer::brewer.pal(length(unique(pfas_group_filtre$substance)), "Set3"),
        labels = paste0(pfas_group_filtre$substance, " (", round(pfas_group_filtre$percentage, 1), "%)")
      ) +
      guides(fill = guide_legend(title = "Substance",
                                 title.position = "top",
                                 label.position = "right",
                                 label.theme = element_text(size = 10),
                                 title.hjust = 0.5))
  })
  
  output$val_total_pfas_region <- renderPlot({
    ggplot(regions_sum_pfas, aes(x = reorder(region, total_value), y = total_value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(x = "Région", y = "Valeur Totale des PFAS", 
           title = "Valeur Totale des PFAS par Région") +
      theme_minimal() +
      # Ajouter les totaux de producteurs et utilisateurs au bout de chaque barre
      geom_text(aes(label = nb_producteur), position = position_stack(vjust = 1.05), 
                color = "black", size = 3) +
      geom_text(aes(label = nb_utilisateurs), position = position_stack(vjust = 1.2), 
                color = "red", size = 3)  # Les utilisateurs en rouge, producteurs en noir
  })
  
  output$evo_substance <- renderPlot({
    ggplot() +
      geom_line(data = year_counts %>% filter(year < max(year_counts$year)), 
                aes(x = year, y = count), color = "steelblue", size = 1) +
      geom_point(data = year_counts %>% filter(year < max(year_counts$year)), 
                 aes(x = year, y = count), color = "steelblue", size = 2) +
      geom_line(data = groupe_annee %>% filter(year < max(groupe_annee$year)), 
                aes(x = year, y = pfas_sum * max(year_counts$count) / max(groupe_annee$pfas_sum)), 
                color = "red", size = 1.2) +
      geom_point(data = groupe_annee %>% filter(year < max(groupe_annee$year)), 
                 aes(x = year, y = pfas_sum * max(year_counts$count) / max(groupe_annee$pfas_sum)), 
                 color = "red", size = 2) +
      scale_y_continuous(
        name = "Nombre de prélèvements",
        sec.axis = sec_axis(~ . * max(groupe_annee$pfas_sum) / max(year_counts$count), name = "Somme des PFAS")
      ) +
      scale_x_continuous(breaks = seq(min(year_counts$year), max(year_counts$year) - 1, by = 2)) +
      labs(title = "Nombre de prélèvements par année et évolution de la quantité de PFAS détectés", x = "Année") +
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "steelblue", size = 12),  
        axis.title.y.right = element_text(color = "red", size = 12)  
      )
  })
  
  output$france_matrix <- renderPlot({
    ggplot(matrix_france, aes(x = matrix, y = pfas_sum, fill = matrix)) +
      geom_bar(stat = "identity", show.legend = FALSE) +  # Utiliser des barres avec les valeurs de pfas_sum
      labs(title = "La quantité de PFAS détectés par milieux", 
           x = "Matrix", 
           y = "Somme des PFAS") +
      theme_minimal() +  # Thème minimal pour le graphique
      scale_fill_brewer(palette = "Set1")  # Palette de couleurs agréables
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
    grid.arrange(p2, nrow = 1)
  })
  
  output$combined_plot2 <- renderPlot({
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
    grid.arrange(p1, nrow = 1)
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
    # Convertir les données en format long pour ggplot2
    france_long <- resultats %>%
      pivot_longer(
        cols = starts_with("pourcentage_non_conformes"),
        names_to = "reglementation",
        values_to = "pourcentage_non_conformes"
      ) %>%
      mutate(reglementation = case_when(
        reglementation == "pourcentage_non_conformes_france" ~ "France",
        reglementation == "pourcentage_non_conformes_danemark" ~ "Danemark",
        reglementation == "pourcentage_non_conformes_usa" ~ "USA"
      ))
    
    # Créer le graphique pour la France
    ggplot(france_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Évolution du pourcentage de prélèvements non conformes en France",
        x = "Année",
        y = "Pourcentage de prélèvements non conformes (%)",
        color = "Réglementation"
      ) +
      scale_color_manual(values = c("France" = "blue", "Danemark" = "red", "USA" = "green")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Nouveau graphique pour la région sélectionnée
  output$region_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    # Filtrer les données pour la région sélectionnée
    region_data <- resultats_region_annee %>%
      filter(region == region_id)
    
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
        y = "Pourcentage de prélèvements non conformes (%)",
        color = "Réglementation"
      ) +
      scale_color_manual(values = c("France" = "blue", "Danemark" = "red", "USA" = "green")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Nouveau graphique pour la France par matrice
  output$france_matrix_plot <- renderPlot({
    # Agréger les données pour la France (toutes régions confondues)
    france_matrix_data <- resultat_conformite_matrix %>%
      group_by(year, matrix) %>%
      summarise(
        pourcentage_france = mean(pourcentage_france, na.rm = TRUE),
        pourcentage_danemark = mean(pourcentage_danemark, na.rm = TRUE),
        pourcentage_usa = mean(pourcentage_usa, na.rm = TRUE)
      )
    
    # Convertir les données en format long pour ggplot2
    france_matrix_long <- france_matrix_data %>%
      pivot_longer(
        cols = starts_with("pourcentage"),
        names_to = "reglementation",
        values_to = "pourcentage_non_conformes"
      ) %>%
      mutate(reglementation = case_when(
        reglementation == "pourcentage_france" ~ "France",
        reglementation == "pourcentage_danemark" ~ "Danemark",
        reglementation == "pourcentage_usa" ~ "USA"
      ))
    
    # Créer le graphique pour la France par matrice
    ggplot(france_matrix_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~matrix) +  # Un graphique par matrice
      labs(
        title = "Évolution du pourcentage de prélèvements non conformes en France par matrice",
        x = "Année",
        y = "Pourcentage de prélèvements non conformes (%)",
        color = "Réglementation"
      ) +
      scale_color_manual(values = c("France" = "blue", "Danemark" = "red", "USA" = "green")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Nouveau graphique pour la région sélectionnée par matrice
  output$region_matrix_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    # Filtrer les données pour la région sélectionnée
    region_matrix_data <- resultat_conformite_matrix %>%
      filter(region == region_id)
    
    # Convertir les données en format long pour ggplot2
    region_matrix_long <- region_matrix_data %>%
      pivot_longer(
        cols = starts_with("pourcentage"),
        names_to = "reglementation",
        values_to = "pourcentage_non_conformes"
      ) %>%
      mutate(reglementation = case_when(
        reglementation == "pourcentage_france" ~ "France",
        reglementation == "pourcentage_danemark" ~ "Danemark",
        reglementation == "pourcentage_usa" ~ "USA"
      ))
    
    # Vérifier si la colonne 'matrix' existe et n'est pas vide
    if (!"matrix" %in% colnames(region_matrix_long) || all(is.na(region_matrix_long$matrix))) {
      return(ggplot() + labs(title = "La colonne 'matrix' est manquante ou vide dans les données."))
    }
    
    # Créer le graphique pour la région sélectionnée par matrice
    ggplot(region_matrix_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~matrix) +  # Un graphique par matrice
      labs(
        title = paste("Évolution du pourcentage de prélèvements non conformes dans", region_id, "par matrice"),
        x = "Année",
        y = "Pourcentage de prélèvements non conformes (%)",
        color = "Réglementation"
      ) +
      scale_color_manual(values = c("France" = "blue", "Danemark" = "red", "USA" = "green")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  region_selected <- reactiveVal(NULL)
  
  # Détecter un clic sur la carte et mettre à jour la région sélectionnée
  observeEvent(input$map_shape_click, {
    region_selected(input$map_shape_click$id)  # Stocker l'ID de la région sélectionnée
    
    # Exécuter un script JS pour ouvrir l'accordéon
    session$sendCustomMessage("openAccordion", list(id = "region_accordion"))
  })
  
  # Texte pour la région sélectionnée
  output$region_name <- renderText({
    req(input$map_shape_click, input$year)
    region_id <- input$map_shape_click$id
    selected_year <- input$year
    
    if (input$ignore_year) {
      region_data <- regions %>%
        filter(nom == region_id)
    } else {
      region_data <- regions_by_year %>%
        filter(nom == region_id, year == selected_year)
    }
    
    if (nrow(region_data) == 0) {
      paste(
        "<b>Région sélectionnée :</b>", region_id, "<br>",
        "<b>Aucune donnée disponible pour l'année :</b>", selected_year
      )
    } else {
      paste(
        "<p><b>Région sélectionnée :</b>", region_data$nom, "</p>",
        "<p><b>Nombre de prélèvements :</b>", region_data$nb_pfas, "</p>",
        "<p><b>Somme des valeurs des PFAS :</b> ", region_data$sum_pfas, "</p>",
        "<p><b>Nombre de producteurs :</b> ", region_data$nb_producteurs, "</p>",
        "<p><b>Nombre d'utilisateurs :</b> ", region_data$nb_utilisateurs, "</p>"
      )
    }
  })
}
