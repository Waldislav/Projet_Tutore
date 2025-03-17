# Charger les fichiers nécessaires
library(shiny)
library(shinythemes)
library(leaflet)
library(scales)
library(bslib)
library(gridExtra)
library(leaflegend)

source("Script/Charge_data/quick_start.R")
print("1")
#source("Charge_data/main.R")
source("Script/page_web/ui.R")
print("2")
source("Script/page_web/server.R")
print("3")
# Lancer l'application Shiny
#shinyApp(ui = ui, server = server)

