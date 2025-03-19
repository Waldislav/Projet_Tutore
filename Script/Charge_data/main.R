library(tidyverse)
library(jsonlite)
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)

# Bien se placer dans le dossier Script pour celui là (la commande setwd("/chemin/vers/script")

# Chargement des données utiles
source("Charge_data/charge_donnees.R")

# Récupération des pfas
source("Charge_data/pfas_donnees.R")

# Calcul de normes 
source("Charge_data/vraies_normes.R")

# Réalises les dataframes nécessaires aux stats par régions
source("Charge_data/stat_region.R")

# Fait des stats sur un rayon de 5km autour d'un user et d'un porducteur 
source("Charge_data/stat_user_producteur.R")

# Sauvegarde nos données nécessaire au fonctionnement de l'application
source("Charge_data/quick_save.R")

