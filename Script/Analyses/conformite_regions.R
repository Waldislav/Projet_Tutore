# Charger les bibliothèques nécessaires
library(dplyr)
library(readr)

# Créer un nouveau dataframe pour les résultats agrégés par région
resultats_region <- france_norme %>%
  group_by(region) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_france = sum(non_conforme_france, na.rm = TRUE),
    pourcentage_non_conformes_france = round(non_conformes_france / total_prelevements * 100, 2),
    non_conformes_danemark = sum(non_conforme_danemark, na.rm = TRUE),
    pourcentage_non_conformes_danemark = round(non_conformes_danemark / total_prelevements * 100, 2),
    non_conformes_usa = sum(non_conforme_usa, na.rm = TRUE),
    pourcentage_non_conformes_usa = round(non_conformes_usa / total_prelevements * 100, 2)
  )


resultats_region_annee <- france_norme %>%
  group_by(region, year) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_france = sum(non_conforme_france, na.rm = TRUE),
    non_conformes_danemark = sum(non_conforme_danemark, na.rm = TRUE),
    non_conformes_usa = sum(non_conforme_usa, na.rm = TRUE)
  ) %>%
  mutate(
    pourcentage_france = (non_conformes_france / total_prelevements) * 100,
    pourcentage_danemark = (non_conformes_danemark / total_prelevements) * 100,
    pourcentage_usa = (non_conformes_usa / total_prelevements) * 100
  )

# Exporter les résultats en CSV
write_csv(resultats_region_annee, file = "../Data/reglementations/resultats_region_annee.csv")
write_csv(resultats_region, file = "../Data/reglementations/resultats_region.csv")


rm(resultats_region)
rm(resultats_region_annee)