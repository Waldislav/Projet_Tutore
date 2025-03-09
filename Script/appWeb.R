# Charger les fichiers nécessaires
library(shiny)
library(shinythemes)
library(leaflet)
library(scales)
library(gridExtra)

source("Charge_data/main.R")
source("page_web/ui.R")
source("page_web/server.R")

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

