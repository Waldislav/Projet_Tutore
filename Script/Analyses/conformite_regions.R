# Charger les bibliothèques nécessaires
library(dplyr)
library(readr)

# Créer un nouveau dataframe pour les résultats agrégés par région
resultats_region <- france_conformite %>%
  group_by(region) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_France = sum(non_conforme_France, na.rm = TRUE),
    pourcentage_non_conformes_France = round(non_conformes_France / total_prelevements * 100, 2),
    non_conformes_Danemark = sum(non_conforme_Danemark, na.rm = TRUE),
    pourcentage_non_conformes_Danemark = round(non_conformes_Danemark / total_prelevements * 100, 2),
    non_conformes_USA = sum(non_conforme_USA, na.rm = TRUE),
    pourcentage_non_conformes_USA = round(non_conformes_USA / total_prelevements * 100, 2)
  )


resultats_region_annee <- france_conformite %>%
  group_by(region, year) %>%
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
write_csv(resultats_region_annee, file = "resultats_region_annee.csv")

rm(resultats_region)
rm(resultats_region_annee)