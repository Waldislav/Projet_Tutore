library(sf)
library(dplyr)

moyenne_globale <- pfas %>%
  summarise(moyenne_value = mean(value, na.rm = TRUE))

moyenne_par_region <- pfas %>%
  group_by(region) %>%
  summarise(moyenne_value = mean(value, na.rm = TRUE))


# Convertir en sf
user_sf <- st_as_sf(user, coords = c("lon", "lat"), crs = 4326)
producteur_sf <- st_as_sf(producteur, coords = c("lon", "lat"), crs = 4326)
pfas_sf <- st_as_sf(pfas, coords = c("lon", "lat"), crs = 4326)

# Convertir en projection UTM pour la distance en mètres
user_sf <- st_transform(user_sf, crs = 32633)
producteur_sf <- st_transform(producteur_sf, crs = 32633)
pfas_sf <- st_transform(pfas_sf, crs = 32633)

# Créer un buffer de 5 km autour des points user et producteur
buffer_5km_user <- st_buffer(user_sf, dist = 5000)
buffer_5km_producteur <- st_buffer(producteur_sf, dist = 5000)

# Fonction pour calculer la moyenne des pfas dans un rayon de 5 km
calculate_mean_pfas <- function(buffer, pfas_sf) {
  # Trouver les pfas dans le buffer
  pfas_in_buffer <- st_intersection(buffer, pfas_sf)
  # Calculer la moyenne des pfas_value
  mean_pfas <- mean(pfas_in_buffer$value, na.rm = TRUE)
  return(mean_pfas)
}

# Ajouter la colonne moyenne_5km aux dataframes user et producteur
user_sf$moyenne_5km <- sapply(1:nrow(user_sf), function(i) calculate_mean_pfas(buffer_5km_user[i,], pfas_sf))
producteur_sf$moyenne_5km <- sapply(1:nrow(producteur_sf), function(i) calculate_mean_pfas(buffer_5km_producteur[i,], pfas_sf))

producteur_sf$moyenne_5km[is.na(producteur_sf$moyenne_5km)] <- 0
user_sf$moyenne_5km[is.na(user_sf$moyenne_5km)] <- 0

user$moyenne_5km <- user_sf$moyenne_5km
producteur$moyenne_5km <- producteur_sf$moyenne_5km

user <- left_join(user, moyenne_par_region, by = "region")
user <- user %>%
  rename(moyenne_par_region = moyenne_value)

producteur <- left_join(producteur, moyenne_par_region, by = "region")
producteur <- producteur %>%
  rename(moyenne_par_region = moyenne_value)

rm(user_sf)
rm(buffer_5km_user)
rm(buffer_5km_producteur)
rm(producteur_sf)
rm(moyenne_par_region)