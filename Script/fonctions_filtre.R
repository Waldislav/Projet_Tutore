library(dplyr)
library(stringr)

complete_lat_lon <- function(df) {
  # Normaliser les noms des villes
  normaliser_nom_ville <- function(nom) {
    nom %>%
      str_to_lower() %>%  # Met en minuscule
      str_replace_all("-", " ") %>%  # Remplace les '-' par un espace
      str_trim()  # Supprime les espaces superflus
  }
  
  # Ajouter une colonne normalisée pour la correspondance
  df <- df %>%
    mutate(city_norm = normaliser_nom_ville(city))  
  
  villes <- villes %>%
    mutate(city_norm = normaliser_nom_ville(label))  # Normalise aussi le dataset des villes
  
  # Supprimer les doublons dans villes en ne gardant qu'une ligne par ville
  villes_unique <- villes %>%
    distinct(city_norm, .keep_all = TRUE)
  
  # Fusionner pour remplir uniquement les lat/lon manquants
  df <- df %>%
    left_join(villes_unique %>% select(city_norm, latitude, longitude), by = "city_norm") %>%
    mutate(
      lat = coalesce(lat, latitude),  # Remplir avec latitude de villes
      lon = coalesce(lon, longitude)  # Remplir avec longitude de villes
    ) %>%
    select(-city_norm, -latitude, -longitude)  # Supprimer les colonnes inutiles
  
  return(df)
}


nettoyer <- function(df) {
  df_clean <- df %>%
    filter(pfas_sum != 0, !is.na(lat), !is.na(lon), country == "France")  # Filtrer les lignes
  return(df_clean)
}
