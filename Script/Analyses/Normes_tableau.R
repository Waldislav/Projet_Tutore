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

# Créer un nouveau dataframe pour les sommes des substances
france_sommes <- france %>%
  mutate(
    somme_France = map_dbl(pfas_values, ~ calculer_somme_substances(.x, substances_concernees$France)),
    somme_Danemark = map_dbl(pfas_values, ~ calculer_somme_substances(.x, substances_concernees$Danemark)),
    somme_USA = map_dbl(pfas_values, ~ calculer_somme_substances(.x, substances_concernees$USA))
  )

# Fonction pour vérifier si un prélèvement dépasse les normes
verifier_normes <- function(somme, matrix, country) {
  # Vérifier si la matrice existe dans les normes du pays
  if (matrix %in% names(normes[[country]])) {
    norme <- normes[[country]][[matrix]]
    return(somme > norme)
  } else {
    # Retourner FALSE si la matrice n'est pas trouvée
    return(FALSE)
  }
}

# Créer un nouveau dataframe pour les vérifications de conformité
france_conformite <- france_sommes %>%
  mutate(
    non_conforme_France = map2_lgl(somme_France, matrix, ~ verifier_normes(.x, .y, "France")),
    non_conforme_Danemark = map2_lgl(somme_Danemark, matrix, ~ verifier_normes(.x, .y, "Danemark")),
    non_conforme_USA = map2_lgl(somme_USA, matrix, ~ verifier_normes(.x, .y, "USA"))
  )

# Créer un nouveau dataframe pour les résultats agrégés
resultats <- france_conformite %>%
  group_by(year) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_France = sum(non_conforme_France, na.rm = TRUE),
    pourcentage_non_conformes_France = round(non_conformes_France / total_prelevements * 100, 2),
    non_conformes_Danemark = sum(non_conforme_Danemark, na.rm = TRUE),
    pourcentage_non_conformes_Danemark = round(non_conformes_Danemark / total_prelevements * 100, 2),
    non_conformes_USA = sum(non_conforme_USA, na.rm = TRUE),
    pourcentage_non_conformes_USA = round(non_conformes_USA / total_prelevements * 100, 2)
)

# Agréger les résultats par région, année et matrice
resultats_conformite_matrix <- france_conformite %>%
  group_by(region, year, matrix) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_France = sum(non_conforme_France, na.rm = TRUE),
    non_conformes_Danemark = sum(non_conforme_Danemark, na.rm = TRUE),
    non_conformes_USA = sum(non_conforme_USA, na.rm = TRUE)
  ) %>%
  mutate(
    pourcentage_France = (non_conformes_France / total_prelevements) * 100,
    pourcentage_Danemark = (non_conformes_Danemark / total_prelevements) * 100,
    pourcentage_USA = (non_conformes_USA / total_prelevements) * 100
  )

# Exporter les résultats en CSV
write_csv(resultats_conformite_matrix, file = "resultat_conformite_matrix.csv")
write_csv(resultats, file = "resultats.csv")

source("Analyses/vraies_normes.R")

# Nettoyer l'environnement (optionnel)
rm(normes)
rm(substances_concernees)
rm(france_sommes)
rm(resultats)