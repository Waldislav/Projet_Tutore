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
  left_join(pfas_stats_by_year, by = c("NAME_1" = "region")) %>%
  left_join(producteurs_by_region, by = c("NAME_1" = "region")) %>%
  left_join(utilisateurs_by_region, by = c("NAME_1" = "region"))

pfas_stats <- pfas %>%
  group_by(region) %>%
  summarise(
    nb_pfas = n(),                    
    sum_pfas = sum(value, na.rm = TRUE)
  )

# Ajouter les statistiques à la table france_regions_sf
regions <- regions %>%
  left_join(pfas_stats, by = c("NAME_1" = "region")) %>%
  left_join(producteurs_by_region, by = c("NAME_1" = "region")) %>%
  left_join(utilisateurs_by_region, by = c("NAME_1" = "region"))

rm(pfas_stats)
rm(pfas_stats_by_year)
rm(producteurs_by_region)
rm(utilisateurs_by_region)
