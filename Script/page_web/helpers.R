calculate_prelevements <- function(region_id) {
  france %>%
    filter(region == region_id, year < 2024) %>%
    group_by(year) %>%
    summarise(count = n(), .groups = "drop")
}

calculate_pfas_total <- function(region_id) {
  pfas %>%
    filter(region == region_id, year < 2024) %>%
    group_by(year) %>%
    summarise(pfas_sum = sum(value, na.rm = TRUE), .groups = "drop")
}

calculate_selected_pfas <- function(region_id, selected_substance) {
  pfas %>%
    filter(region == region_id, substance == selected_substance, year < 2024) %>%
    group_by(year) %>%
    summarise(selected_pfas_sum = sum(value, na.rm = TRUE), .groups = "drop")
}