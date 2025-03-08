library(tidyverse)
library(jsonlite)
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer) # Pour des couleurs pas trop saturés

# Bien se placer dans le dossier de nos scripts (la commande setwd("/chemin/vers/script") dans le terminal pour se placer correctement)
#setwd("C:/Users/Ronan/Documents/GitHub/Projet_Tutore/Script")
#setwd("/Users/waldislav/Fac/Cours/M2/Projet\ Tutorés/Projet_Tutore/Script")
#setwd("C://Users/ronan/Documents/Cours/Projet_Tutore-main/Projet_Tutore-main/Script")

# Chargement des données utiles
source("charge_donnees.R")

# Récupération des pfas
source("pfas_donnees.R")

# Réalises les dataframes nécessaires aux stats par régions
source("stat_region.R")