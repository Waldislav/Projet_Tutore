# Charger les packages nécessaires
library(tidyverse)
library(sf)

# Définir le chemin absolu vers le fichier GeoPackage
file_path <- "../gadm41_FRA.gpkg"

# Charger les données géographiques des régions françaises depuis ADM_ADM_1
france_regions_sf <- st_read(file_path, layer = "ADM_ADM_1")

# Afficher les informations sur les données chargées
print("Colonnes disponibles dans la couche des régions:")
print(names(france_regions_sf))

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

# Vérifier la structure du dataframe final
print("\nStructure du dataframe final:")
str(france_with_regions_df)

# Afficher les premières lignes pour vérification
print("\nAperçu des données finales:")
print(head(france_with_regions_df))

# Exporter le dataframe final en CSV
write.csv(france_with_regions_df, "france_with_regions.csv", row.names = FALSE)

rm(france_regions_sf)
rm(france_sf)
rm(france_with_regions_sf)
rm(lat_lon_coords)