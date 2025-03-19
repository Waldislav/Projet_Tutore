ui <- page_fluid(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(rel = "icon", href = "favicon.ico", type = "image/x-icon"),
    tags$title("Étude PFAS"),
    tags$script(src = "script.js")
  ),
  includeHTML("Script/page_web/modules/baniere.html"),
  includeHTML("Script/page_web/modules/apropos.html"),
  tags$div(id = "analyse-globale",
    class = "section",
    titlePanel("Analyse globale"),
    tags$p("Ici nous avons une analyse totale sur toute la France. Les prélèvements sont tous réunis entre 2006 et 2024. La somme des PFAS sur une année est faite en ng/L ou ng/kg."),
    card(
      card_header("Évolution de toutes les substances"),
      plotOutput("evo_substance"),
      layout_sidebar(
        sidebar = sidebar(
          selectInput("une_substance", "Choisir une substance : ",
                      unique(pfas$substance))
        ),
        plotOutput("evo_une_substance")
      )
    ),
    tags$p("On peut également voir comment nos données sont répandues sur nos régions et combien d'utilisateur (en rouge) et de producteur (en noir) sont présents par régions. On peut également voir la diversité des PFAS analysés, les 4.7% d'Autres représentent 30 PFAS différents."),
    layout_columns(
      card(
        card_header("Répartition des PFAS par région"),
        plotOutput("val_total_pfas_region")
      ),
      card(
        card_header("Répartition des PFAS"),
        plotOutput("cam_pfas_plot")  # Ajout du plot ici
      )
    ),
    tags$p("Les réglementation sont différentes dans chaques pays et chaque milieu dans lequel le prélévement a été effectué a sa propre réglementation."),
    card(
      card_header("Les réglementations en France"),
      plotOutput("france_plot"),    # Nouveau graphique pour la France
      plotOutput("france_matrix_plot")   # Nouveau graphique pour la France par matrice
    )
  ),
  tags$div(id = "analyse-geographique",
    class = "section",
    titlePanel("Analyses spatio-temporelles"),
    tags$p("Une carte intéractive est fournie pour des analyses plus précise. Un clic sur une région permet d'afficher des détails sur elle dans la partie 'Analyse régionale'."),
    card(
      card_header("Carte de la France"),
      sidebarLayout(
        sidebarPanel(
          selectInput("substance", "Choisir une substance :",
                      c("Tous", unique(pfas$substance))),
          selectInput("matrix", "Choisir une milieu :",
                      c("Tous", unique(pfas$matrix))),
          selectInput("regle", "Choisir une réglementation :",
                      c("France", "USA", "Danemark")),
          sliderInput("year", "Année :",
                      min = min(pfas$year, na.rm = TRUE),
                      max = max(pfas$year, na.rm = TRUE),
                      value = min(pfas$year, na.rm = TRUE),
                      step = 1,
                      sep = ""),
          checkboxInput("ignore_year", "Ignorer l'année", value = FALSE)
        ),
        mainPanel(
          leafletOutput("map"),
        )
      )),
    accordion(
      id = "prelevement_accordion",
      open = FALSE,
      accordion_panel(
        "Prélèvements",
        card(
          selectInput("date", "Choisir une date : ", choices = NULL),
          tableOutput("table_pfas")
        )
      )
    ),
    accordion(
      id = "region_accordion",
      open = FALSE,
      accordion_panel(
        "Analyse régionale",
        card(
          card_header("Informations région"),
          uiOutput("region_name")
        ),
        card(
          card_header("Analyses globales sur la région"),
          layout_columns(
            plotOutput("box_plot"),       # Sortie pour le nouveau graphique en bougies
            plotOutput("combined_plot2"),  # Sortie pour le graphique combiné existant  
          ),
          layout_sidebar(
            sidebar = sidebar(
              checkboxInput("show_prelevements", "Afficher les prélèvements", value = TRUE),
              checkboxInput("show_pfas_total", "Afficher la somme des PFAS", value = TRUE),
              checkboxInput("show_selected_pfas", "Afficher la substance sélectionnée", value = TRUE)
            ),
            plotOutput("combined_plot"),  # Sortie pour le graphique combiné existant
          ),
        ),
        card(
          card_header("Conformités de la region"),
          plotOutput("region_plot"),    # Nouveau graphique pour la région sélectionnée
          plotOutput("region_matrix_plot")   # Nouveau graphique pour la région sélectionnée par matrice
        )
      )
    ),
  ),
  includeHTML("Script/page_web/modules/info.html")
)
