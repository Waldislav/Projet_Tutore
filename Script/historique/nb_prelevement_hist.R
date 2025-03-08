library(ggplot2)

# Vérifier que le dataframe existe
if (exists("france")) {
  # Compter le nombre de lignes par année
  year_counts <- as.data.frame(table(france$year))
  colnames(year_counts) <- c("year", "count")
  
  # Convertir l'année en numérique si nécessaire
  year_counts$year <- as.numeric(as.character(year_counts$year))
  
  # Afficher la courbe
  ggplot(year_counts, aes(x = year, y = count)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(title = "Nombre de lignes par année", x = "Année", y = "Nombre de lignes") +
    theme_minimal()
} else {
  print("Le dataframe 'france' n'existe pas.")
}