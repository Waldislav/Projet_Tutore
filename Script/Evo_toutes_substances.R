library(tidyverse)
library(lubridate)

# Ajouter la colonne 'years' et 'less_than' et effectuer un groupement par année
pfas_summary <- pfas_df %>%
  #left_join(parsed_data %>% select(rowid, year), by = "rowid") %>%  # Associer 'year' au dataframe pfas_df
  group_by(year, substance) %>%  # Groupement par les colonnes 'year' et 'substance'
  summarise(
    avg_value = mean(final_value, na.rm = TRUE),  # Calcul de la moyenne des valeurs
    # avg_less_than = mean(from_less_than, na.rm = TRUE),  # Moyenne de 'from_less_than' si pertinent
    .groups = "drop"
  )

# Étape 2 : Visualisation de l'évolution des valeurs moyennes
ggplot(pfas_summary, aes(x = year, y = avg_value, color = substance)) +
  geom_line() +  # Tracer la courbe pour chaque substance
  geom_point() +  # Ajouter des points pour mieux visualiser les données
  labs(
    title = "Évolution des valeurs moyennes par substance",
    x = "Année",
    y = "Valeur Moyenne"
  ) +
  theme_minimal()

