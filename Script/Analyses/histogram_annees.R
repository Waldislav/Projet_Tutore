library(ggplot2)

# Regroupe le total des pfas par années
groupe_annee <- aggregate(pfas_sum ~ year, data = france, FUN = sum)

# Créer le graphique de l'évolution de pfas_sum par année
ggplot(groupe_annee, aes(x = year, y = pfas_sum)) +
  geom_line(group = 1, color = brewer.pal(3, "Set1")[2], linewidth = 1.2) +  # Ligne bleue pour l'évolution
  geom_point(color = brewer.pal(3, "Set1")[1], size = 2) +  # Points rouges pour chaque valeur
  labs(title = "Évolution de la quantité de PFAS détectés en France au fil du temps",
       x = "Année", 
       y = "Somme des PFAS") +
  theme_minimal() +  # Un thème minimal pour le graphique
  theme(axis.text = element_text(size = 12),  # Taille des étiquettes
        axis.title = element_text(size = 14))

# Regroupe le total des pfas par milieux
matrix_france <- aggregate(pfas_sum ~ matrix, data = france, FUN = sum)

ggplot(france, aes(x = matrix, fill = matrix)) +
  geom_bar(show.legend = FALSE) +  # Compte les occurrences de chaque "matrix"
  labs(title = "Nombre d'occurrences de chaque milieu",       
       x = "Matrix",       
       y = "Nombre d'occurrences") +
  theme_minimal() +  
  scale_fill_brewer(palette = "Set1") +  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Évite l'écrasement visuel en ajoutant un léger espace en haut


rm(matrix_france)
rm(groupe_annee)

