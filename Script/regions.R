# Charger les packages nécessaires
library(tidyverse)
library(sf)

# chemin fichier GeoPackage
file_path <- "C:/Users/Ronan/Documents/Cours/M2/Projet_tut_V2/Script/gadm41_FRA.gpkg"

# Charger les données géographiques des régions françaises depuis ADM_ADM_1 (c est les régions)
france_regions_sf <- st_read(file_path, layer = "ADM_ADM_1")


# Simplifier le jeu de données des régions en ne gardant que le nom et la géométrie
france_regions_sf <- france_regions_sf %>% 
  select(REGION_NAME = NAME_1, geom)

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

# Afficher les premières lignes
print("\nAperçu des données finales:")
print(head(france_with_regions_df))

# Exporter le dataframe final en CSV
write.csv(france_with_regions_df, "france_with_regions.csv", row.names = FALSE)