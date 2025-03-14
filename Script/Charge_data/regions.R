# Charger les données géographiques des régions françaises depuis ADM_ADM_1
france_regions_sf <- st_read("../regions.gpkg")

# Simplifier le jeu de données des régions en ne gardant que le nom et la géométrie
france_regions_sf <- france_regions_sf %>% 
  select(REGION_NAME = nom, geom)

# Sauvegarder les coordonnées avant la conversion en sf
lat_lon_coords <- france %>% 
  select(lat, lon)

# Convertir le dataframe 'france' en objet sf tout en préservant toutes les colonnes
france_sf <- st_as_sf(france, coords = c("lon", "lat"), crs = 4326)

# Effectuer la jointure spatiale
france_with_regions_sf <- st_join(france_sf, france_regions_sf, join = st_intersects)

# Convertir en dataframe tout en préservant toutes les colonnes et ajouter lat/lon
france_with_regions_df <- france_with_regions_sf %>%
  st_drop_geometry() %>%
  rename(region = REGION_NAME) %>%
  bind_cols(lat_lon_coords)

france <- france_with_regions_df

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


rm(producteur_count)
rm(utilisateur_count)
rm(france_with_regions_df)
rm(france_regions_sf)
rm(france_sf)
rm(france_with_regions_sf)
rm(lat_lon_coords)