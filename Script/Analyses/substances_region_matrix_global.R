# Étape 1 : Sélectionner les colonnes nécessaires
pfas_data <- pfas %>%
  filter(substance %in% names(sort(table(substance), decreasing = TRUE))[1:9]) %>%
  select(region, substance, matrix, value, year) %>%
  mutate(value = as.numeric(value))  


# Étape 3 : Créer le diagramme en barres empilées
ggplot(pfas_data, aes(x = matrix, y = value, fill = substance)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Quantité de PFAS détectée par milieu et par substance",
    x = "Milieu (Matrix)",
    y = "Quantité de PFAS détectée",
    fill = "Substance"
  ) +
  theme_minimal() +  # Style épuré
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
