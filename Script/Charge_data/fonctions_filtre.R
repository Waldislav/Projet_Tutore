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
  
  # Filtrer les pays où ADMIN == "France"
  france_geom <- pays %>%
    filter(ADMIN == "France") %>%
    st_geometry()
  
  # Créer un objet sf temporaire pour la vérification de l'intersection
  df_points_sf <- st_as_sf(df_points, coords = c("lon", "lat"), crs = 4326)
  
  # Vérifier si chaque point est dans la France et obtenir un vecteur logique
  is_in_france <- st_intersects(df_points_sf, france_geom, sparse = FALSE)
  
  # Remplir la colonne "country" uniquement si elle est NA
  df_points$country <- ifelse(is.na(df_points$country) & rowSums(is_in_france) > 0, "France", df_points$country)
  
  # Retourner le dataframe avec les colonnes lat, lon et country
  return(df_points)
}


nettoyer <- function(df) {
  df_clean <- df %>%
    filter(pfas_sum != 0, !is.na(lat), !is.na(lon), country == "France")  # Filtrer les lignes
  return(df_clean)
}
