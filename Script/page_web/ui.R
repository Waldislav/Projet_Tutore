ui <- page_fluid(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$title("Étude PFAS"),
    tags$script(src = "script.js")
  ),
  includeHTML("page_web/modules/baniere.html"),
  includeHTML("page_web/modules/apropos.html"),
  tags$div(id = "analyse-globale",
    class = "section",
    titlePanel("Analyse globale"),
    tags$p("Ici nous avons notre répartition de pfas"),
    layout_columns(
      card(
        card_header("Répartition des PFAS"),
        plotOutput("cam_pfas_plot")  # Ajout du plot ici
      )
    )
  ),
  tags$div(id = "analyse-geographique",
    class = "section",
    titlePanel("Analyse géographique"),
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
        plotOutput("region_plot"),    # Nouveau graphique pour la région sélectionnée
        plotOutput("france_matrix_plot"),  # Nouveau graphique pour la France par matrice
        plotOutput("region_matrix_plot")   # Nouveau graphique pour la région sélectionnée par matrice
      )
    ),
    includeHTML("page_web/modules/info.html"),
  )
)