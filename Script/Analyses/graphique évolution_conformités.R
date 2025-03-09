# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)

# Agréger les données par année pour les régions (regroupées)
resultats_regions_annee <- france_conformite %>%
  group_by(year) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_France = sum(non_conforme_France, na.rm = TRUE),
    non_conformes_Danemark = sum(non_conforme_Danemark, na.rm = TRUE),
    non_conformes_USA = sum(non_conforme_USA, na.rm = TRUE)
  ) %>%
  mutate(
    region = "Régions",  # Ajouter une colonne "region" pour les régions regroupées
    pourcentage_France = (non_conformes_France / total_prelevements) * 100,
    pourcentage_Danemark = (non_conformes_Danemark / total_prelevements) * 100,
    pourcentage_USA = (non_conformes_USA / total_prelevements) * 100
  )

# Agréger les données pour la France entière
resultats_france_annee <- france_conformite %>%
  group_by(year) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_France = sum(non_conforme_France, na.rm = TRUE),
    non_conformes_Danemark = sum(non_conforme_Danemark, na.rm = TRUE),
    non_conformes_USA = sum(non_conforme_USA, na.rm = TRUE)
  ) %>%
  mutate(
    region = "France",  # Ajouter une colonne "region" pour la France
    pourcentage_France = (non_conformes_France / total_prelevements) * 100,
    pourcentage_Danemark = (non_conformes_Danemark / total_prelevements) * 100,
    pourcentage_USA = (non_conformes_USA / total_prelevements) * 100
  )

# Combiner les données pour la France et les régions
donnees_graphique <- bind_rows(resultats_regions_annee, resultats_france_annee)

# Convertir les données en format long pour ggplot2
donnees_long <- donnees_graphique %>%
  pivot_longer(
    cols = starts_with("pourcentage"),
    names_to = "reglementation",
    values_to = "pourcentage_non_conformes"
  ) %>%
  mutate(reglementation = case_when(
    reglementation == "pourcentage_France" ~ "France",
    reglementation == "pourcentage_Danemark" ~ "Danemark",
    reglementation == "pourcentage_USA" ~ "USA"
  ))

# Créer le graphique pour la France et les régions regroupées
ggplot(donnees_long, aes(x = year, y = pourcentage_non_conformes, color = reglementation, linetype = region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Évolution du pourcentage de prélèvements non conformes par année",
    subtitle = "Comparaison des réglementations France, Danemark et USA",
    x = "Année",
    y = "Pourcentage de prélèvements non conformes",
    color = "Réglementation",
    linetype = "Zone"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = unique(donnees_long$year))  # Afficher toutes les années sur l'axe des x


# Agréger les données par année et par région
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

# Convertir les données en format long pour ggplot2
donnees_long_regions <- resultats_region_annee %>%
  pivot_longer(
    cols = starts_with("pourcentage"),
    names_to = "reglementation",
    values_to = "pourcentage_non_conformes"
  ) %>%
  mutate(reglementation = case_when(
    reglementation == "pourcentage_France" ~ "France",
    reglementation == "pourcentage_Danemark" ~ "Danemark",
    reglementation == "pourcentage_USA" ~ "USA"
  ))

# Créer le graphique pour les régions
ggplot(donnees_long_regions, aes(x = year, y = pourcentage_non_conformes, color = reglementation)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~region) +  # Un graphique par région
  labs(
    title = "Évolution du pourcentage de prélèvements non conformes par année et par région",
    x = "Année",
    y = "Pourcentage de prélèvements non conformes",
    color = "Réglementation"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = unique(donnees_long_regions$year))  # Afficher toutes les années sur l'axe des x

rm(resultats_regions_annee)
rm(resultats_region_annee)
rm(resultats_france_annee)
rm(donnees_graphique)
rm(donnees_long)
rm(donnees_long_regions)