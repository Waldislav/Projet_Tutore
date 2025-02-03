# Chargement des données nécessaire aux traitements 

regions <- st_read("../gadm41_FRA.gpkg", layer = "ADM_ADM_1")
villes <- read_csv("../Data/villes.csv")

# Fonctions de traitements
source("fonctions_filtre.R")


# Chargement des données d'utilisateurs et producteurs de pfas
# 0
user <- read_csv("../Data/user.csv")
# 13
producteur <- read_csv("../Data/producteur.csv")

producteur <- producteur %>% filter(country == "France")
user <- user %>% filter(country == "France")

# Chargement des données de pfas détéctés
# 12
surface_water <- read_csv("../Data/surface_water.csv")
# 28
ades <- read_csv("../Data/ades.csv")    
# 29
naiades <- read_csv("../Data/naiades.csv")
# 30
anses <- read_csv("../Data/anses.csv")
# 31
rhine <- read_csv("../Data/rhine.csv")
# 124
eaurob <- read_csv("../Data/france_eaurob.csv")
# 131
radiofrance_dw <- read_csv("../Data/radiofrance_dw.csv")
# 132
tfa <- read_csv("../Data/tfa.csv")

surface_water <- complete_city_country(surface_water)
tfa <- complete_city_country(tfa)

france <- bind_rows(ades, naiades, anses, rhine, eaurob, radiofrance_dw, surface_water, tfa)
france <- complete_lat_lon(france)

# On filtre toutes les valeurs non significatives
france <- nettoyer(france)

# On ajoute les regions à france
source("regions.R")

# On supprime les dataframes innutilisés 
rm(ades)
rm(naiades)
rm(anses)
rm(rhine)
rm(eaurob)
rm(radiofrance_dw)
rm(surface_water)
rm(tfa)