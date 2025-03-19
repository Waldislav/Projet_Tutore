# Définition des normes
normes_france <- list(
  drinking_water_total_pfas = 500,
  drinking_water_sum_20_pfas = 100,
  underground_water_pfos = 2500,
  surface_water_pfos = 2500
)

normes_danemark <- list(
  underground_water_pfos = 2,
  drinking_water_sum_4_pfas = 2,
  surface_water_pfos = 0.65
)

# Normes américaines (limites individuelles en ng/L)
normes_usa <- list(
  PFOA = 4,
  PFOS = 4,
  PFHxS = 10,
  PFNA = 10,
  PFBS = 10
)

# Fonction de vérification
verifier_conformite <- function(matrix_type, pfas_sum, pfas_values) {
  non_conforme_france <- FALSE
  non_conforme_danemark <- FALSE
  non_conforme_usa <- FALSE
  
  substances_20_pfas <- c("PFOS", "PFOA", "PFNA", "PFDA", "PFUnDA", "PFDoDA", "PFHxS",
                          "PFHpA", "PFBS", "PFPeA", "PFHxA", "PFHpS", "PFOSA", "PFNS",
                          "PFDS", "PFTeDA", "PFTrDA", "PFDoA", "PFUnA", "PFHxDA")
  
  substances_4_pfas <- c("PFOA", "PFOS", "PFNA", "PFHxS")
  
  # Vérification des normes françaises
  if (matrix_type == "Drinking water") {
    if (!is.na(pfas_sum) && pfas_sum > normes_france$drinking_water_total_pfas) {
      non_conforme_france <- TRUE
    }
    
    sum_20_pfas <- pfas_values %>%
      filter(substance %in% substances_20_pfas) %>%
      summarise(sum_20 = sum(value, na.rm = TRUE)) %>%
      pull(sum_20)
    
    if (!is.na(sum_20_pfas) && sum_20_pfas > normes_france$drinking_water_sum_20_pfas) {
      non_conforme_france <- TRUE
    }
    
    sum_4_pfas <- pfas_values %>%
      filter(substance %in% substances_4_pfas) %>%
      summarise(sum_4 = sum(value, na.rm = TRUE)) %>%
      pull(sum_4)
    
    if (!is.na(sum_4_pfas) && sum_4_pfas > normes_danemark$drinking_water_sum_4_pfas) {
      non_conforme_danemark <- TRUE
    }
  }
  
  if (matrix_type == "Underground water") {
    pfos_value <- pfas_values %>% filter(substance == "PFOS") %>% pull(value)
    
    if (any(pfos_value > normes_france$underground_water_pfos, na.rm = TRUE)) {
      non_conforme_france <- TRUE
    }
    if (any(pfos_value > normes_danemark$underground_water_pfos, na.rm = TRUE)) {
      non_conforme_danemark <- TRUE
    }
  }
  
  if (matrix_type == "Surface water") {
    pfos_value <- pfas_values %>% filter(substance == "PFOS") %>% pull(value)
    
    if (any(pfos_value > normes_france$surface_water_pfos, na.rm = TRUE)) {
      non_conforme_france <- TRUE
    }
    if (any(pfos_value > normes_danemark$surface_water_pfos, na.rm = TRUE)) {
      non_conforme_danemark <- TRUE
    }
  }
  
  # Vérification des normes américaines (uniquement pour Drinking water, Underground water, et Surface water)
  if (matrix_type %in% c("Drinking water", "Underground water", "Surface water")) {
    for (substance in names(normes_usa)) {
      substance_value <- pfas_values %>% filter(substance == !!substance) %>% pull(value)
      if (any(substance_value > normes_usa[[substance]], na.rm = TRUE)) {
        non_conforme_usa <- TRUE
        break  # Si une substance dépasse la limite, on marque comme non conforme
      }
    }
  }
  
  return(list(non_conforme_france = non_conforme_france, 
              non_conforme_danemark = non_conforme_danemark,
              non_conforme_usa = non_conforme_usa))
}

# Vérifier que la colonne 'matrix' est présente dans 'france'
if (!"matrix" %in% colnames(france)) {
  stop("La colonne 'matrix' n'est pas présente dans le dataframe 'france'. Veuillez vérifier les noms des colonnes.")
}

# Appliquer la fonction de vérification ligne par ligne
france_norme <- france %>%
  mutate(conformite = pmap(list(matrix, pfas_sum, rowid),
                           ~ verifier_conformite(..1, ..2, pfas %>% filter(rowid == ..3)))) %>%
  unnest_wider(conformite)

# Exporter le dataframe en CSV
#write.csv(france_norme, "france_norme.csv", row.names = FALSE)

# Afficher le dataframe final
print(france_norme)

# Créer un nouveau dataframe pour les résultats agrégés
resultats <- france_norme %>%
  group_by(year) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_france = sum(non_conforme_france, na.rm = TRUE),
    pourcentage_non_conformes_france = round(non_conformes_france / total_prelevements * 100, 2),
    non_conformes_danemark = sum(non_conforme_danemark, na.rm = TRUE),
    pourcentage_non_conformes_danemark = round(non_conformes_danemark / total_prelevements * 100, 2),
    non_conformes_usa = sum(non_conforme_usa, na.rm = TRUE),
    pourcentage_non_conformes_usa = round(non_conformes_usa / total_prelevements * 100, 2)
  )

# Agréger les résultats par région, année et matrice
resultat_conformite_matrix <- france_norme %>%
  group_by(region, year, matrix) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_france = sum(non_conforme_france, na.rm = TRUE),
    non_conformes_danemark = sum(non_conforme_danemark, na.rm = TRUE),
    non_conformes_usa = sum(non_conforme_usa, na.rm = TRUE)
  ) %>%
  mutate(
    pourcentage_france = (non_conformes_france / total_prelevements) * 100,
    pourcentage_danemark = (non_conformes_danemark / total_prelevements) * 100,
    pourcentage_usa = (non_conformes_usa / total_prelevements) * 100
  )

resultats_region_annee <- france_norme %>%
  group_by(region, year) %>%
  summarise(
    total_prelevements = n(),
    non_conformes_france = sum(non_conforme_france, na.rm = TRUE),
    non_conformes_danemark = sum(non_conforme_danemark, na.rm = TRUE),
    non_conformes_usa = sum(non_conforme_usa, na.rm = TRUE)
  ) %>%
  mutate(
    pourcentage_France = (non_conformes_france / total_prelevements) * 100,
    pourcentage_Danemark = (non_conformes_danemark / total_prelevements) * 100,
    pourcentage_USA = (non_conformes_usa / total_prelevements) * 100
  )

