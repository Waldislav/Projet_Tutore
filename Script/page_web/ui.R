ui <- fluidPage(
  titlePanel("PFAS par Région"),
  sidebarLayout(
    sidebarPanel(
      selectInput("substance", "Choisir une substance :",
                  choices = unique(pfas$substance)),
      sliderInput("year", "Année :",
                  min = min(pfas$year, na.rm = TRUE),
                  max = max(pfas$year, na.rm = TRUE),
                  value = min(pfas$year, na.rm = TRUE),
                  step = 1,
                  sep = ""),
      checkboxInput("ignore_year", "Ignorer l'année", value = FALSE),
      checkboxInput("show_prelevements", "Afficher les prélèvements", value = TRUE),
      checkboxInput("show_pfas_total", "Afficher la somme des PFAS", value = TRUE),
      checkboxInput("show_selected_pfas", "Afficher la substance sélectionnée", value = TRUE),
      uiOutput("region_name")
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("combined_plot"),  # Sortie pour le graphique combiné existant
      plotOutput("box_plot"),       # Sortie pour le nouveau graphique en bougies
      plotOutput("france_plot"),    # Nouveau graphique pour la France
      plotOutput("region_plot")     # Nouveau graphique pour la région sélectionnée
    )
  )
)