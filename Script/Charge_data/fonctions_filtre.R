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

complete_country <- function(df_points) {
  # Vérifier s'il y a des valeurs manquantes dans les colonnes lat et lon
  df_points <- df_points %>%
    filter(!is.na(lat) & !is.na(lon))  # Supprimer les lignes avec NA
  
  # Convertir les points en objet sf
  points_sf <- st_as_sf(df_points, coords = c("lon", "lat"), crs = 4326)
  
  # Vérifier l'intersection entre les points et les régions
  is_in_region <- st_within(points_sf, regions$geom, sparse = FALSE)
  
  # Ajouter la colonne country
  df_points$country <- ifelse(rowSums(is_in_region) > 0, "France", ifelse(df_points$source_text == "Naiades", "France", NA))
  
  return(df_points)
}


nettoyer <- function(df) {
  df_clean <- df %>%
    filter(pfas_sum != 0, !is.na(lat), !is.na(lon), country == "France")  # Filtrer les lignes
  return(df_clean)
}
