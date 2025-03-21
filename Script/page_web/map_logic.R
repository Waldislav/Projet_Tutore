library(htmlwidgets)


addLegendCustom <- function(map, position = "bottomright", colors, labels, sizes) {
  legend_html <- paste0(
    "<div style='background-color: white; padding: 5px; border-radius: 5px;'>",
    "<strong>Légende</strong><br>"
  )
  
  for (i in seq_along(colors)) {
    legend_html <- paste0(
      legend_html,
      "<div style='display: flex; align-items: center; margin-bottom: 3px;'>",
      "<div style='width:", sizes[i], "px; height:", sizes[i], "px; ",
      "background-color:", colors[i], "; border-radius: 50%; ",
      "margin-right: 5px;'></div>",
      labels[i], "</div>"
    )
  }
  
  legend_html <- paste0(legend_html, "</div>")
  
  return(addControl(map, HTML(legend_html), position = position))
}

create_map <- function(input, data, regle) {
  leaflet(data, options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addPolygons(
      data = regions,
      layerId = ~nom,  # Identifier les polygones par leur nom
      fillColor = "transparent", # Couleur initiale (transparent)
      color = "white",    # Couleur de la bordure
      weight = 2,         # Épaisseur de la bordure
      fillOpacity = 0.0,  # Opacité initiale
      highlightOptions = highlightOptions(
        weight = 3,       # Épaissir la bordure au survol
        color = "blue",   # Bordure bleue au survol
        fillColor = "blue", # Remplissage bleu au survol
        fillOpacity = 0.2  # Opacité plus élevée au survol # Mettre la forme au premier plan
      )
    ) %>%
    addCircleMarkers(
      data = data,
      ~lon, ~lat,
      layerId = ~rowid,
      color = "black",
      opacity = 1,
      weight = 1,
      fillColor = ~ifelse(
        regle == "France" & non_conforme_france == TRUE, "red",
        ifelse(regle == "USA" & non_conforme_usa == TRUE, "red",
               ifelse(regle == "Danemark" & non_conforme_danemark == TRUE, "red", "orange"))),
      fillOpacity = 1,
      radius = 5,
      popup = ~paste(
        "Id :", rowid, "<br>",
        "Milieu :", matrix, "<br>",
        "Région : ", region, "<br>",
        "Ville : ", city, "<br>",
        "Sommes des PFAS : ", pfas_sum, " ", unit, "<br>",
        "Année:", year, "<br>"
        #,tableau
      )
    ) %>%
    addCircleMarkers(
      data = user,
      ~lon, ~lat,
      color = "grey",
      radius = 3,
      fillColor = "grey",
      fillOpacity = 1,
      popup = ~paste(
        "Utilisateur : ", name, "<br>",
        "Région : ", region, "<br>",
        "Moyenne des valeurs de PFAS national : ", round(moyenne_globale$moyenne_value[1], digits = 2), "<br>",
        "Moyenne des valeurs de PFAS de la région : ", round(moyenne_par_region, digits = 2), "<br>",
        "Moyenne des valeurs de PFAS dans un rayon de 5km : ", round(moyenne_5km, digits = 2)
      )
    ) %>%
    addCircleMarkers(
      data = producteur,
      ~lon, ~lat,
      color = "black",
      radius = 3,
      fillColor = "black",
      fillOpacity = 1,
      popup = ~paste(
        "Producteur : ", name, "<br>",
        "Région : ", region, "<br>",
        "Moyenne des valeurs de PFAS national : ", round(moyenne_globale$moyenne_value[1], digits = 2), "<br>",
        "Moyenne des valeurs de PFAS de la région : ", round(moyenne_par_region, digits = 2), "<br>",
        "Moyenne des valeurs de PFAS dans un rayon de 5km : ", round(moyenne_5km, digits = 2)
      )
    ) %>%
    addLegendCustom(
      position = "bottomright",
      colors = c("orange", "red", "grey", "black"),
      labels = c("Prélévement conforme","Prélévement non conforme", "Utilisateur", "Producteur"),
      sizes = c(10, 10, 10, 10) # Taille des cercles
    )
}


