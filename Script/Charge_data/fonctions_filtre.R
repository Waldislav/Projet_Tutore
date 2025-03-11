library(dplyr)
library(stringr)

complete_lat_lon <- function(df) {

  normaliser_nom_ville <- function(nom) {
    nom %>%
      str_to_lower() %>%
      str_replace_all("-", " ") %>%
      str_trim()
  }
  
  # Ajouter une colonne normalisée pour la correspondance
  df <- df %>%
    mutate(city_norm = normaliser_nom_ville(city))  
  
  villes <- villes %>%
    mutate(city_norm = normaliser_nom_ville(label))
  
  # Supprimer les doublons dans villes en ne gardant qu'une ligne par ville
  villes_unique <- villes %>%
    distinct(city_norm, .keep_all = TRUE)
  
  # Fusionner pour remplir uniquement les lat/lon manquants
  df <- df %>%
    left_join(villes_unique %>% select(city_norm, latitude, longitude), by = "city_norm") %>%
    mutate(
      lat = coalesce(lat, latitude),
      lon = coalesce(lon, longitude)
    ) %>%
    select(-city_norm, -latitude, -longitude)
  
  return(df)
}

library(sf)

complete_country <- function(df, region) {
  
  df_valid <- df %>% filter(!is.na(lat) & !is.na(lon))
  
  # Transformer df en objet spatial sf
  df_sf <- st_as_sf(df_valid, coords = c("lon", "lat"), crs = 4326) # WGS84
  
  # Vérifier si les points sont dans une des régions
  is_in_france <- sapply(st_intersects(df_sf, regions$geom), function(x) length(x) > 0)
  
  # Mettre à jour seulement les country à NA
  df$country[is.na(df$country) & is_in_france] <- "France"
  
  return(df)
}


nettoyer <- function(df) {
  df_clean <- df %>%
    filter(pfas_sum != 0, !is.na(lat), !is.na(lon), country == "France")  # Filtrer les lignes
  return(df_clean)
}
