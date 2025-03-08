library(tidyverse)
library(lubridate)

# Étape 1 : Ajouter la colonne 'years' et 'less_than' et effectuer un groupement par année
pfas_summary <- pfas %>%
  left_join(france %>% select(rowid, year_col = year), by = "rowid") %>%  # Associer 'years' au dataframe pfas_df et renommer la colonne 'year' en 'year_col'
  group_by(year_col, substance) %>%  # Groupement par année et substance
  summarise(
    avg_value = sum(value, na.rm = TRUE),  # Calcul de la moyenne des valeurs
    .groups = "drop"
  ) %>%
  filter(substance == "PFOS")  # Filtrer pour ne conserver que la substance PFOS

# Étape 2 : Visualisation de l'évolution des valeurs moyennes pour une seule substance
ggplot(pfas_summary, aes(x = year_col, y = avg_value)) +
  geom_line() +  # Tracer la courbe
  geom_point() +  # Ajouter des points pour mieux visualiser les données
  labs(
    title = "Évolution des valeurs cumulées pour les PFOS",
    x = "Année",
    y = "Valeur totale"
  ) +
  theme_minimal()
