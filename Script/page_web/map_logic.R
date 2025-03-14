create_map <- function(data) {
  leaflet(data) %>%
    addTiles() %>%
    addPolygons(
      data = regions,
      layerId = ~nom,
      fillColor = "transparent",
      color = "white",
      weight = 2,
      fillOpacity = 0.0,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.2
      )
    ) %>%
    addCircleMarkers(
      ~lon, ~lat,
      color = ~substance,
      fillColor = "red",
      opacity = ~rescale(value, to = c(0.1, 1)),
      popup = ~paste(
        "ID :", rowid, "<br>",
        "Région:", region, "<br>",
        "Ville:", city, "<br>",
        "Substance:", substance, "<br>",
        "Valeur PFAS:", value, "<br>",
        "Année:", year
      )
    ) %>%
    addCircleMarkers(
      data = user,
      ~lon, ~lat,
      color = "grey",
      radius = 5,
      fillColor = "grey",
      fillOpacity = 1,
      popup = ~paste("Utilisateur : ", name)
    ) %>%
    addCircleMarkers(
      data = producteur,
      ~lon, ~lat,
      color = "black",
      radius = 5,
      fillColor = "black",
      fillOpacity = 1,
      popup = ~paste("Producteur : ", name)
    )
}