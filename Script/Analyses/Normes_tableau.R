# Charger les bibliothèques nécessaires
library(dplyr)
library(jsonlite)
library(purrr)

# Définir les normes PFAS réelles pour chaque pays et type de matrice
normes <- list(
  France = list(
    Biota = 1000,  # Pas de norme spécifique pour le biote en France
    Groundwater = 1000,  # Pas de norme spécifique pour les eaux souterraines en France
    Sediment = 1000,  # Pas de norme spécifique pour les sédiments en France
    Surface_water = 0.65,  # Norme de qualité environnementale pour le PFOS dans les eaux de surface
    Drinking_water = 100  # Limite de qualité pour la somme de 20 PFAS dans l'eau potable
  ),
  
  
  Danemark = list(
    Biota = 2,  # 2 ng/kg pour Biota
    Groundwater = 2,  # 2 ng/L pour les eaux souterraines
    Sediment = 2,  # 2 ng/kg pour les sédiments
    Surface_water = 2,  # 2 ng/L pour les eaux de surface
    Drinking_water = 2  # 2 ng/L pour les eaux potables
  ),
  USA = list(
    Biota = 70,  # 70 ng/kg pour Biota
    Groundwater = 70,  # 70 ng/L pour les eaux souterraines
    Sediment = 70,  # 70 ng/kg pour les sédiments
    Surface_water = 70,  # 70 ng/L pour les eaux de surface
    Drinking_water = 4  # 4 ng/L pour les eaux potables (recommandation EPA)
  )
)

# Substances concernées pour chaque pays
substances_concernees <- list(
  France = c("PFOA", "PFOS", "PFNA", "PFHxS"),  # Tous les PFAS
  Danemark = c("PFOA", "PFOS", "PFNA", "PFHxS"),  # PFOA, PFOS, PFNA, PFHxS
  USA = c("PFOA", "PFOS")  # PFOA et PFOS
)

# Fonction pour extraire et sommer les substances concernées
calculer_somme_substances <- function(pfas_values, substances) {
  valeurs <- fromJSON(pfas_values)
  
  # Convertir la colonne 'value' en numérique
  valeurs <- valeurs %>%
    mutate(value = as.numeric(value))
  
  # Filtrer et sommer les substances concernées
  somme <- valeurs %>%
    filter(substance %in% substances) %>%
    summarise(somme = sum(value, na.rm = TRUE)) %>%
    pull(somme)
  
  return(somme)
}





source("Analyses/vraies_normes.R")

# Nettoyer l'environnement (optionnel)
rm(normes)
rm(substances_concernees)
rm(france_sommes)
rm(resultats)