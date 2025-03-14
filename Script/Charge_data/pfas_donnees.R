# Parsing des données
france <- france %>% 
  rowwise() %>%
  rowid_to_column() %>%
  #mutate(pfas_values = list(fromJSON(pfas_values))) %>%
  ungroup()

# Récupère tout le tableau de pfas_values
restmp <- france %>%
  rowwise() %>%
  mutate(parsed_values = list(fromJSON(pfas_values))) %>%
  unnest(parsed_values, names_sep = "_") %>%             
  filter(!is.null(parsed_values_value)) %>%
  mutate(rowid = rowid, 
         parsed_values_value = as.numeric(parsed_values_value),
         parsed_values_less_than = as.numeric(parsed_values_less_than)) %>% 
  select(rowid, region, city, date, year, lat, lon, matrix, starts_with("parsed_values_")) %>%
  rename_with(~ gsub("^parsed_values_", "", .), starts_with("parsed_values_"))

restmp <- as.data.frame(restmp)

# Tableau de pfas avec une valeur significative
pfas <- restmp[!is.na(restmp$value), ] %>%
  mutate(value = as.numeric(value))

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


producteur_count <- producteur %>%
  group_by(region) %>%
  summarise(nb_producteur = n())

utilisateur_count <- user %>%
  group_by(region) %>%
  summarise(nb_utilisateurs = n())

# Regrouper les données PFAS par région et année (comme avant)
regions_sum_pfas <- pfas %>%
  group_by(region) %>%
  filter(!is.na(region)) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Somme des valeurs PFAS par région et année
    nb_pfas = n()                            # Nombre de PFAS par région et année
  ) %>%
  ungroup()

regions_sum_pfas <- regions_sum_pfas %>%
  left_join(producteur_count, by = c("region" = "region")) %>%
  left_join(utilisateur_count, by = c("region" = "region"))

# Tableau temporaire, on le supprime pour ne pas surcharger
rm(producteur_count)
rm(utilisateur_count)
rm(restmp)
