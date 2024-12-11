library(tidyverse)
library(rjson)
library(leaflet)

leaflet(pfas_df) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    color = ~ifelse(from_less_than, "orange", "blue"),  # Orange si less_than, bleu sinon
    popup = ~paste(
      "Substance:", substance, "<br>",
      "Valeur finale:", final_value, "<br>",
      ifelse(from_less_than, "Origine: Less Than", "Origine: Value")
    ),
    radius = ~ifelse(is.na(final_value), 5, (final_value / max(final_value, na.rm = TRUE)) * 10),
    fillOpacity = 0.7,
    stroke = FALSE
  ) %>%
  addLegend("bottomright", 
            colors = c("blue", "orange"),
            labels = c("Valeur", "Less Than"),
            title = "Source des données")
