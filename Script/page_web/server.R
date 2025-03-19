# Charger les fichiers de logique spécifique
source("Script/page_web/map_logic.R")
source("Script/page_web/plot_logic.R")

server <- function(input, output, session) {
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
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "La quantité de PFAS détectés par milieux", 
           x = "Matrix", 
           y = "Somme des PFAS") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  })
  
  # Graphique combiné existant
  output$combined_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    selected_substance <- input$substance
    
    pfas_data <- pfas %>%
      filter(region == region_id,
             substance %in% names(sort(table(substance), decreasing = TRUE))[1:9]) %>%
      select(region, substance, matrix, value) %>%
      mutate(value = as.numeric(value))
    
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
    
    p2 <- create_combined_plot(region_id, selected_substance, input$show_prelevements, input$show_pfas_total, input$show_selected_pfas)
    
    grid.arrange(p2, nrow = 1)
  })
  
  output$combined_plot2 <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    selected_substance <- input$substance
    
    pfas_data <- pfas %>%
      filter(region == region_id,
             substance %in% names(sort(table(substance), decreasing = TRUE))[1:9]) %>%
      select(region, substance, matrix, value) %>%
      mutate(value = as.numeric(value))
    
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
    
    p2 <- create_combined_plot(region_id, selected_substance, input$show_prelevements, input$show_pfas_total, input$show_selected_pfas)
    
    grid.arrange(p1, nrow = 1)
  })
  
  output$box_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    france_count <- france %>%
      filter(region == region_id) %>%
      count(matrix)
    
    ggplot(france_count, aes(x = matrix, y = n, fill = matrix)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste("Nombre de lignes par milieu (Matrix) dans", region_id),
        x = "Milieu (Matrix)",
        y = "Nombre de lignes",
        fill = "Milieu"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$france_plot <- renderPlot({
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
  
  output$region_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    region_data <- resultats_region_annee %>%
      filter(region == region_id)
    
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
  
  output$france_matrix_plot <- renderPlot({
    france_matrix_data <- resultat_conformite_matrix %>%
      group_by(year, matrix) %>%
      summarise(
        pourcentage_france = mean(pourcentage_france, na.rm = TRUE),
        pourcentage_danemark = mean(pourcentage_danemark, na.rm = TRUE),
        pourcentage_usa = mean(pourcentage_usa, na.rm = TRUE)
      )
    
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
    
    ggplot(france_matrix_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~matrix) +
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
  
  output$evo_une_substance <- renderPlot({
    pfas_summary <- pfas %>%
      filter(substance == input$une_substance) %>% 
      left_join(france %>% select(rowid, year_col = year), by = "rowid") %>%
      group_by(year_col, substance) %>% 
      summarise(
        avg_value = sum(value, na.rm = TRUE),
        nb_valeur = n(),  
        .groups = "drop"
      )
    
    ggplot() +
      geom_line(data = pfas_summary, 
                aes(x = year_col, y = nb_valeur), color = "steelblue", size = 1) +
      geom_point(data = pfas_summary,
                 aes(x = year_col, y = nb_valeur), color = "steelblue", size = 2) +
      geom_line(data = pfas_summary,
                aes(x = year_col, y = avg_value * max(pfas_summary$nb_valeur) / max(pfas_summary$avg_value)), 
                color = "red", size = 1.2) +
      geom_point(data = pfas_summary, 
                 aes(x = year_col, y = avg_value * max(pfas_summary$nb_valeur) / max(pfas_summary$avg_value)), 
                 color = "red", size = 2) +
      scale_y_continuous(
        name = "Nombre de prélèvements",
        sec.axis = sec_axis(~ . * max(pfas_summary$avg_value) / max(pfas_summary$nb_valeur), name = "Somme des PFAS")
      ) +
      scale_x_continuous(
        breaks = if (length(unique(pfas_summary$year_col)) > 1) {
          seq(min(pfas_summary$year_col), max(pfas_summary$year_col) - 1, by = 2)
        } else {
          unique(pfas_summary$year_col)
        }
      ) +
      labs(title = paste("Nombre de prélèvements par année et évolution de la quantité de", input$une_substance), x = "Année") +
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "steelblue", size = 12),  
        axis.title.y.right = element_text(color = "red", size = 12)  
      )
    
  })
  
  output$region_matrix_plot <- renderPlot({
    req(input$map_shape_click)
    
    region_id <- input$map_shape_click$id
    
    region_matrix_data <- resultat_conformite_matrix %>%
      filter(region == region_id)
    
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
    
    if (!"matrix" %in% colnames(region_matrix_long) || all(is.na(region_matrix_long$matrix))) {
      return(ggplot() + labs(title = "La colonne 'matrix' est manquante dans les données."))
    }
    
    ggplot(region_matrix_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~matrix) +
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
  
  observeEvent(input$map_shape_click, {
    region_selected(input$map_shape_click$id) 
  })
  
  selected_dates <- reactive({
    req(input$map_marker_click)
    
    lat <- input$map_marker_click$lat
    lon <- input$map_marker_click$lng
    print(paste(paste("lat : ",lat),paste(" lon :", lon)))

    dates <- unique(france_norme %>% 
                      filter(lat == !!lat, lon == !!lon) %>% 
                      mutate(date_year = paste(format(date, "%Y-%m-%d"), year, sep = " - ")) %>%
                      pull(date_year))
    return(dates)
  })
  
  observe({
    req(input$map_marker_click)
    
    lat <- input$map_marker_click$lat
    lon <- input$map_marker_click$lng

    fr_filtre <- france_norme %>% 
      filter(lat == !!lat, lon == !!lon) %>% 
      mutate(date_year = (paste(format(date, "%Y-%m-%d"), year, sep = " - "))) %>%
      arrange(desc(date))
    
    if (!input$ignore_year) {
      fr_filtre <- fr_filtre %>%
        filter(year == input$year)
    }
    
    updateSelectInput(session, "date", choices = setNames(fr_filtre$rowid, fr_filtre$date_year))
  })
  
  output$table_pfas <- renderTable({
    id_prelev <- input$date
    pfas %>%
      filter(rowid == id_prelev) %>%
      select(substance, value, unit)
  })
  
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
        "<p><b>Nombre de PFAS détectés :</b>", region_data$nb_pfas, "</p>",
        "<p><b>Somme des valeurs des PFAS :</b> ", region_data$sum_pfas, "</p>",
        "<p><b>Nombre de producteurs :</b> ", region_data$nb_producteurs, "</p>",
        "<p><b>Nombre d'utilisateurs :</b> ", region_data$nb_utilisateurs, "</p>"
      )
    }
  })
}