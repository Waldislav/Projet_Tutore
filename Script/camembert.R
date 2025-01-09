library(rjson)
library(ggplot2)
library(RColorBrewer) # Pour des couleurs pas trop saturés

# Récupère tout le tableau de pfas_values
restmp <- parsed_data %>%
  rowwise() %>%
  mutate(parsed_values = list(fromJSON(pfas_values))) %>% # Convertir pfas_values en liste R
  unnest(parsed_values, names_sep = "_") %>%             # Désimbriquer les JSON avec un préfixe pour les colonnes
  filter(!is.null(parsed_values_value)) %>%              # Garder uniquement les entrées où "value" n'est pas NULL
  mutate(rowid = rowid) %>%                              # Ajouter le "rowid" d'origine
  select(rowid, starts_with("parsed_values_")) %>%          # Réorganiser les colonnes 
  rename_with(~ gsub("^parsed_values_", "", .), starts_with("parsed_values_"))

restmp <- as.data.frame(restmp)

# Tableau de pfas avec une valeur significative
pfas <- restmp[!is.na(restmp$value), ] %>%
  mutate(value = as.numeric(value))

# Tableau temporaire, on le supprime pour ne pas surcharger
rm(restmp)

# Groupe les pfas entre eux et y ajoute un pourcentage de répartition
pfas_group <- pfas %>%
  group_by(substance) %>%
  summarise(total_value = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = total_value / sum(total_value) * 100)

# Filtre le regroupement des pfas en ajoutant ceux qui ont un pourcentage inférieur à 1% dans la valeur "Autres"
pfas_group_filtre <- pfas_group %>%
  mutate(substance = ifelse(percentage < 1, "Autres", substance)) %>%
  group_by(substance) %>%
  summarise(total_value = sum(total_value), .groups = 'drop') %>%
  mutate(percentage = total_value / sum(total_value) * 100) %>%
  arrange(desc(percentage))

# Trie les groupes par leur %
pfas_group_filtre$substance <- factor(pfas_group_filtre$substance, levels = pfas_group_filtre$substance[order(-pfas_group_filtre$percentage)])

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
