source("page_web/helpers.R")  # Charger les fonctions utilitaires

create_combined_plot <- function(region_id, selected_substance, show_prelevements, show_pfas_total, show_selected_pfas) {
  req(region_id)
  
  # Calculer les données nécessaires
  year_counts <- calculate_prelevements(region_id)
  pfas_summary <- calculate_pfas_total(region_id)
  selected_pfas_summary <- calculate_selected_pfas(region_id, selected_substance)
  
  # Adapter les échelles
  scaling_factor <- max(year_counts$count) / max(pfas_summary$pfas_sum)
  
  # Créer le graphique combiné
  p <- ggplot() +
    scale_y_continuous(
      name = "Nombre de prélèvements",
      sec.axis = sec_axis(~ . / scaling_factor, name = "Somme des PFAS")
    ) +
    scale_x_continuous(breaks = seq(min(year_counts$year), max(year_counts$year), by = 2)) +
    labs(
      title = paste("Nombre de prélèvements, somme des PFAS et", selected_substance, "par année dans", region_id),
      x = "Année",
      y = "Nombre de prélèvements",
      color = "Légende"
    ) +
    theme_minimal()
  
  # Ajouter les courbes en fonction des cases cochées
  if (show_prelevements) {
    p <- p +
      geom_line(data = year_counts, aes(x = year, y = count, color = "Prélèvements"), size = 1) +
      geom_point(data = year_counts, aes(x = year, y = count, color = "Prélèvements"), size = 2)
  }
  
  if (show_pfas_total) {
    p <- p +
      geom_line(data = pfas_summary, aes(x = year, y = pfas_sum * scaling_factor, color = "Somme des PFAS"), size = 1) +
      geom_point(data = pfas_summary, aes(x = year, y = pfas_sum * scaling_factor, color = "Somme des PFAS"), size = 2)
  }
  
  if (show_selected_pfas) {
    p <- p +
      geom_line(data = selected_pfas_summary, aes(x = year, y = selected_pfas_sum * scaling_factor, color = selected_substance), size = 1) +
      geom_point(data = selected_pfas_summary, aes(x = year, y = selected_pfas_sum * scaling_factor, color = selected_substance), size = 2)
  }
  
  # Définir les couleurs pour la légende
  colors <- c()
  if (show_prelevements) colors <- c(colors, "Prélèvements" = "steelblue")
  if (show_pfas_total) colors <- c(colors, "Somme des PFAS" = "red")
  if (show_selected_pfas) colors <- c(colors, selected_substance = "green")
  
  p + scale_color_manual(values = colors)
}