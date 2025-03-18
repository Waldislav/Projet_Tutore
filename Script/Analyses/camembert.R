library(jsonlite)

pfas_group_filtre$substance <- factor(pfas_group_filtre$substance, 
                                      levels = unique(pfas_group_filtre$substance))

# Cambembert sans le pourcentage affiché dedans
ggplot(pfas_group_filtre, aes(x = "", y = total_value, fill = substance)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Répartition des PFAS") +
  geom_text(aes(label = ""),                # Supprime les étiquettes des secteurs
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(length(unique(pfas_group_filtre$substance)), "Set3"),
    labels = paste0(pfas_group_filtre$substance, " (", round(pfas_group_filtre$percentage, 1), "%)")
  ) +
  guides(fill = guide_legend(title = "Substance",
                             title.position = "top",
                             label.position = "right",
                             label.theme = element_text(size = 10),
                             title.hjust = 0.5))

# Camembert avec le pourcentage affiché dedans
ggplot(pfas_group_filtre, aes(x = "", y = total_value, fill = substance)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Répartition des PFAS") +
  geom_text(aes(label = ifelse(percentage > 4, paste0(round(percentage, 1), "%"), "")),  # Condition sur les pourcentages
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values = brewer.pal(length(unique(pfas_group_filtre$substance)), "Set3"),
    labels = paste0(pfas_group_filtre$substance, " (", round(pfas_group_filtre$percentage, 1), "%)")
  ) +
  guides(fill = guide_legend(title = "Substance",
                             title.position = "top",
                             label.position = "right",
                             label.theme = element_text(size = 10),
                             title.hjust = 0.5))

# Ananlyse sur le nombre de pfas par region avec le nombre de producteur (en noir) et d'utilisateurs (en rouge)

producteur_count <- producteur %>%
  group_by(region) %>%
  summarise(nb_producteur = n())

utilisateur_count <- user %>%
  group_by(region) %>%
  summarise(nb_utilisateurs = n())

# Regrouper les données PFAS par région et année (comme avant)
aggregated_data <- pfas %>%
  group_by(region) %>%
  filter(!is.na(region)) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Somme des valeurs PFAS par région et année
    nb_pfas = n()                            # Nombre de PFAS par région et année
  ) %>%
  ungroup()

aggregated_data <- aggregated_data %>%
  left_join(producteur_count, by = c("region" = "region")) %>%
  left_join(utilisateur_count, by = c("region" = "region"))

# Visualisation avec ggplot
ggplot(aggregated_data, aes(x = reorder(region, total_value), y = total_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Région", y = "Valeur Totale des PFAS", 
       title = "Valeur Totale des PFAS par Région") +
  theme_minimal() +
  # Ajouter les totaux de producteurs et utilisateurs au bout de chaque barre
  geom_text(aes(label = nb_producteur), position = position_stack(vjust = 1.05), 
            color = "black", size = 3) +
  geom_text(aes(label = nb_utilisateurs), position = position_stack(vjust = 1.2), 
            color = "red", size = 3)  # Les utilisateurs en rouge, producteurs en noir

rm(producteur_count)
rm(utilisateur_count)
rm(aggregated_data)

