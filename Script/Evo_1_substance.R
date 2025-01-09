library(tidyverse)
library(lubridate)

# Étape 1 : Ajouter la colonne 'years' et 'less_than' et effectuer un groupement par année
pfas_summary <- pfas_df %>%
  left_join(parsed_data %>% select(rowid, year), by = "rowid") %>%  # Associer 'years' au dataframe pfas_df
  group_by(year = year, substance) %>%  # Groupement par année et substance
  summarise(
    avg_value = mean(value, na.rm = TRUE),  # Calcul de la moyenne des valeurs
    .groups = "drop"
  ) %>%
  
  
  filter(substance == "PFHxA")  # Filtrer pour ne conserver que la substance 

# Étape 2 : Visualisation de l'évolution des valeurs moyennes pour une seule substance
ggplot(pfas_summary, aes(x = year, y = avg_value)) +
  geom_line() +  # Tracer la courbe 
  geom_point() +  # Ajouter des points pour mieux visualiser les données
  labs(
    title = "Évolution des valeurs moyennes pour PFHxA",
    x = "Année",
    y = "Valeur Moyenne"
  ) +
  theme_minimal()
