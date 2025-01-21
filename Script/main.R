library(tidyverse)
library(rjson)

# Pour charger nos csv (la commande setwd("/chemin/vers/script") dans le terminal pour se placer correctement)
france <- read_csv("../Data/france.csv")
user <- read_csv("../Data/user.csv")
producteur <- read_csv("../Data/producteur.csv")

# Parsing des données
parsed_data <- france %>% 
  # sample_n(10000) %>%
  rowwise() %>%
  rowid_to_column() %>%
  mutate(parsed = list(fromJSON(pfas_values))) %>%
  ungroup()

producteur <- producteur %>%
  filter(country == "France")

user <- user %>%
  filter(country == "France")

pfas_df <- parsed_data %>% 
  unnest_longer(parsed) %>%
  select(rowid,lat, lon, year, data = parsed) %>%
  mutate(
 # Extraction longitude
    lat = lat,  # Extraction latitude
    lon = lon, 
    value = as.numeric(map_chr(data, "value", .default = NA_character_)),  # Extraction de value
    less_than = as.numeric(map_chr(data, "less_than", .default = NA_character_)),  # Extraction de less_than
    year = year,  # Extraction année
    substance = map_chr(data, "substance", .default = NA_character_)   # Extraction substance
  ) %>%
  mutate(
    final_value = ifelse(!is.na(value), value, less_than),  # Prendre la valeur disponible
    from_less_than = ifelse(is.na(value) & !is.na(less_than), TRUE, FALSE)  # Indicateur
  ) %>%
  select(rowid, lat, lon, final_value, from_less_than, year, substance)
