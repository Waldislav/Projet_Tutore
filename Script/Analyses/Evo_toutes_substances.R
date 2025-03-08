library(ggplot2)
library(tidyverse)
library(lubridate)

# Vérifier que le dataframe existe
if (exists("france")) {
  # Filtrer les données pour exclure 2024
  france <- france %>% filter(year < 2024)
  
  # Compter le nombre de lignes par année
  year_counts <- as.data.frame(table(france$year))
  colnames(year_counts) <- c("year", "count")
  
  # Convertir l'année en numérique si nécessaire
  year_counts$year <- as.numeric(as.character(year_counts$year))
  
  # Regroupe le total des pfas par années
  groupe_annee <- aggregate(pfas_sum ~ year, data = france, FUN = sum)
  
  # Création du graphique
  ggplot() +
    geom_line(data = year_counts, aes(x = year, y = count), color = "steelblue", size = 1) +
    geom_point(data = year_counts, aes(x = year, y = count), color = "steelblue", size = 2) +
    geom_line(data = groupe_annee, aes(x = year, y = pfas_sum * max(year_counts$count) / max(groupe_annee$pfas_sum)), color = "red", size = 1.2) +
    geom_point(data = groupe_annee, aes(x = year, y = pfas_sum * max(year_counts$count) / max(groupe_annee$pfas_sum)), color = "red", size = 2) +
    scale_y_continuous(
      name = "Nombre de prélèvements",
      sec.axis = sec_axis(~ . * max(groupe_annee$pfas_sum) / max(year_counts$count), name = "Somme des PFAS")
    ) +
    scale_x_continuous(breaks = seq(min(year_counts$year), max(year_counts$year), by = 2)) +
    labs(title = "Nombre de prélevements par année et évolution de la quantité de PFAS détectés", x = "Année") +
    theme_minimal()
} else {
  print("Le dataframe 'france' n'existe pas.")
}