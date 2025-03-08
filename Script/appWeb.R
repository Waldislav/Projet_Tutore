# Charger les fichiers nécessaires
source("Charge_data/main.R")
source("page_web/ui.R")
source("page_web/server.R")

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)