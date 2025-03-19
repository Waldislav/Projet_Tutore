pfas_stats_by_year <- pfas %>%
  group_by(region, year) %>%
  summarise(
    nb_pfas = n(),                   
    sum_pfas = sum(value, na.rm = TRUE)
  ) %>%
  ungroup()

producteurs_by_region <- producteur %>%
  group_by(region) %>%
  summarise(nb_producteurs = n())

utilisateurs_by_region <- user %>%
  group_by(region) %>%
  summarise(nb_utilisateurs = n())

# Joindre les statistiques des producteurs et utilisateurs à france_regions_sf_by_year
regions_by_year <- regions %>%
  left_join(pfas_stats_by_year, by = c("nom" = "region")) %>%
  left_join(producteurs_by_region, by = c("nom" = "region")) %>%
  left_join(utilisateurs_by_region, by = c("nom" = "region")) %>%
  mutate(across(c(nb_producteurs, nb_utilisateurs, nb_pfas, sum_pfas), ~ replace(., is.na(.), 0)))


pfas_stats <- pfas %>%
  group_by(region) %>%
  summarise(
    nb_pfas = n(),                    
    sum_pfas = sum(value, na.rm = TRUE)
  )

# Ajouter les statistiques à la table france_regions_sf
regions <- regions %>%
  left_join(pfas_stats, by = c("nom" = "region")) %>%
  left_join(producteurs_by_region, by = c("nom" = "region")) %>%
  left_join(utilisateurs_by_region, by = c("nom" = "region")) %>%
  mutate(across(c(nb_producteurs, nb_utilisateurs, nb_pfas, sum_pfas), ~ replace(., is.na(.), 0)))


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

france_conformite_subset <- france_norme %>%
  select(rowid, non_conforme_france, non_conforme_usa, non_conforme_danemark)

pfas <- pfas %>%
  left_join(france_conformite_subset, by = "rowid")

# Tableau temporaire, on le supprime pour ne pas surcharger
rm(producteur_count)
rm(utilisateur_count)
rm(pfas_stats)
rm(pfas_stats_by_year)
rm(producteurs_by_region)
rm(utilisateurs_by_region)
