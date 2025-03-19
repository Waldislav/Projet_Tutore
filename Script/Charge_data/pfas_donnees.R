# Tous les prélévements fait en France
france <- france %>%
  filter(pfas_sum != 0) %>%
  rowid_to_column()

# Récupère tout le tableau de pfas_values
restmp <- france %>%
  rowwise() %>%
  mutate(parsed_values = list(fromJSON(pfas_values))) %>%
  unnest(parsed_values, names_sep = "_") %>%             
  filter(!is.null(parsed_values_value)) %>%
  mutate(parsed_values_value = as.numeric(parsed_values_value),
         parsed_values_less_than = as.numeric(parsed_values_less_than)) %>% 
  select(rowid, region, city, matrix, date, year, lat, lon, starts_with("parsed_values_")) %>%
  rename_with(~ gsub("^parsed_values_", "", .), starts_with("parsed_values_"))
restmp <- as.data.frame(restmp)

# Tous les pfas avec une valeur significative
pfas <- restmp[!is.na(restmp$value), ]

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

rm(restmp)
