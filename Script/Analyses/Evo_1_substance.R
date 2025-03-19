library(lubridate)

pfas_summary <- pfas %>%
  left_join(france %>% select(rowid, year_col = year), by = "rowid") %>%
  group_by(year_col, substance) %>% 
  summarise(
    avg_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(substance == "PFOS")

ggplot(pfas_summary, aes(x = year_col, y = avg_value)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Évolution des valeurs cumulées pour les PFOS",
    x = "Année",
    y = "Valeur totale"
  ) +
  theme_minimal()
