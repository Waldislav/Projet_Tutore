library(shiny)
library(shinythemes)
library(leaflet)
library(scales)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
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
      # Cases à cocher pour les courbes
      checkboxInput("show_prelevements", "Afficher les prélèvements", value = TRUE),
      checkboxInput("show_pfas_total", "Afficher la somme des PFAS", value = TRUE),
      checkboxInput("show_selected_pfas", "Afficher la substance sélectionnée", value = TRUE),
      uiOutput("region_name")
    ),
    mainPanel(
      leafletOutput("map"),  # Carte Leaflet
      plotOutput("combined_plot")  # Graphique combiné
    )
  )
)

server <- function(input, output) {
  # Données filtrées pour la carte
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
  
  # Carte Leaflet
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
          fillOpacity = 0.2  # Opacité plus élevée au survol
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
  
  # Graphique combiné
  output$combined_plot <- renderPlot({
    req(input$map_shape_click)  # Attendre qu'une région soit sélectionnée
    
    region_id <- input$map_shape_click$id  # Récupérer la région sélectionnée
    selected_substance <- input$substance  # Récupérer la substance sélectionnée
    
    # Filtrer les données en fonction de la région sélectionnée
    region_data <- france %>%
      filter(region == region_id, year < 2024)  # Exclure 2024
    
    # Compter le nombre de prélèvements par année
    year_counts <- as.data.frame(table(region_data$year))
    colnames(year_counts) <- c("year", "count")
    year_counts$year <- as.numeric(as.character(year_counts$year))
    
    # Calculer la somme des PFAS par année
    pfas_summary <- pfas %>%
      filter(region == region_id) %>%
      group_by(year) %>%
      summarise(
        pfas_sum = sum(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculer la somme des PFAS pour la substance sélectionnée
    selected_pfas_summary <- pfas %>%
      filter(region == region_id, substance == selected_substance) %>%
      group_by(year) %>%
      summarise(
        selected_pfas_sum = sum(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Adapter les échelles pour les trois courbes
    scaling_factor <- max(year_counts$count) / max(pfas_summary$pfas_sum)
    
    # Créer le graphique combiné
    p <- ggplot() +
      # Échelles et axes
      scale_y_continuous(
        name = "Nombre de prélèvements",
        sec.axis = sec_axis(~ . / scaling_factor, name = "Somme des PFAS")
      ) +
      scale_x_continuous(breaks = seq(min(year_counts$year), max(year_counts$year), by = 2)) +
      labs(
        title = paste("Nombre de prélèvements, somme des PFAS et", selected_substance, "par année dans", region_id),
        x = "Année",
        y = "Nombre de prélèvements",
        color = "Légende"
      ) +
      theme_minimal()
    
    # Ajouter les courbes en fonction des cases cochées
    if (input$show_prelevements) {
      p <- p +
        geom_line(data = year_counts, aes(x = year, y = count, color = "Prélèvements"), size = 1) +
        geom_point(data = year_counts, aes(x = year, y = count, color = "Prélèvements"), size = 2)
    }
    
    if (input$show_pfas_total) {
      p <- p +
        geom_line(data = pfas_summary, aes(x = year, y = pfas_sum * scaling_factor, color = "Somme des PFAS"), size = 1) +
        geom_point(data = pfas_summary, aes(x = year, y = pfas_sum * scaling_factor, color = "Somme des PFAS"), size = 2)
    }
    
    if (input$show_selected_pfas) {
      p <- p +
        geom_line(data = selected_pfas_summary, aes(x = year, y = selected_pfas_sum * scaling_factor, color = selected_substance), size = 1) +
        geom_point(data = selected_pfas_summary, aes(x = year, y = selected_pfas_sum * scaling_factor, color = selected_substance), size = 2)
    }
    
    # Définir les couleurs pour la légende
    colors <- c()
    if (input$show_prelevements) colors <- c(colors, "Prélèvements" = "steelblue")
    if (input$show_pfas_total) colors <- c(colors, "Somme des PFAS" = "red")
    if (input$show_selected_pfas) colors <- c(colors, selected_substance = "green")
    
    # Ajouter les couleurs à la légende
    p + scale_color_manual(values = colors)
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

shinyApp(ui, server)