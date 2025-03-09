# Charger les librairies nécessaires
library(ggplot2)
library(dplyr)

# Étape 1 : Sélectionner les colonnes nécessaires
pfas_data <- pfas %>%
  filter(substance %in% names(sort(table(substance), decreasing = TRUE))[1:9]) %>%  # Filtrer les 9 substances les plus fréquentes
  select(region, substance, matrix, value, year) %>%
  mutate(value = as.numeric(value))  


# Étape 3 : Créer le diagramme en barres empilées
ggplot(pfas_data, aes(x = matrix, y = value, fill = substance)) +
  geom_bar(stat = "identity", position = "stack") +  # Barres empilées
  labs(
    title = "Quantité de PFAS détectée par milieu et par substance",
    x = "Milieu (Matrix)",
    y = "Quantité de PFAS détectée",
    fill = "Substance"
  ) +
  theme_minimal() +  # Style épuré
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotation des noms des matrices
  scale_fill_brewer(palette = "Set1")  # Palette de couleurs
