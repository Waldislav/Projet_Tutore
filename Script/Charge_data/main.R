library(tidyverse)
library(jsonlite)
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer) # Pour des couleurs pas trop saturés

# Bien se placer dans le dossier de nos scripts (la commande setwd("/chemin/vers/script") dans le terminal pour se placer correctement)
#setwd("C:/Users/Ronan/Documents/GitHub/Projet_Tutore/Script")
setwd("/Users/waldislav/Fac/Cours/M2/Projet\ Tutorés/Projet_Tutore/Script")

# Chargement des données utiles
source("Charge_data/charge_donnees.R")

# Récupération des pfas
source("Charge_data/pfas_donnees.R")

# Réalises les dataframes nécessaires aux stats par régions
source("Charge_data/stat_region.R")