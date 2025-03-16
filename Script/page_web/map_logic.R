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
  leaflet(data) %>%
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
      color = "black",
      opacity = 1,
      weight = 1,
      fillColor = ~ifelse(
        regle == "France" & non_conforme_france == TRUE, "red",  # Si "France" et non conforme, rouge
        ifelse(regle == "USA" & non_conforme_usa == TRUE, "red",  # Si "USA" et non conforme, rouge
               ifelse(regle == "Danemark" & non_conforme_danemark == TRUE, "red", "orange"))),
      fillOpacity = 1,
      radius = 5,
      popup = ~paste(
        "Id :", rowid, "<br>",
        "Milieu :", matrix, "<br>",
        "Région:", region, "<br>",
        "Ville:", city, "<br>",
        #"Substance:", substance, "<br>",
        #"Sommes pfas:", pfas_sum, "<br>",
        "Année:", year, "<br>",
        tableau
      )
    ) %>%
    addCircleMarkers(
      data = user,
      ~lon, ~lat,
      color = "grey",
      radius = 3,
      fillColor = "grey",
      fillOpacity = 1,
      popup = ~paste("Utilisateur : ", name)
    ) %>%
    addCircleMarkers(
      data = producteur,
      ~lon, ~lat,
      color = "black",
      radius = 3,
      fillColor = "black",
      fillOpacity = 1,
      popup = ~paste("Producteur : ", name)
    ) %>%
    addLegendCustom(
      position = "bottomright",
      colors = c("orange", "red", "grey", "black"),
      labels = c("Prélévement conforme","Prélévement non conforme", "Utilisateur", "Producteur"),
      sizes = c(10, 10, 10, 10) # Taille des cercles
    ) %>%
    # Injecter du JS pour charger Turf.js et filtrer les points
    onRender("
      function(el, x) {
        // Charger Turf.js
        var script = document.createElement('script');
        script.src = 'https://cdnjs.cloudflare.com/ajax/libs/Turf.js/6.5.0/turf.min.js';
        script.onload = function() {
          console.log('Turf.js chargé !');
          
          // Exemple : Filtrer les points à moins de 10 km d'un centre donné
          var center = turf.point([2.3522, 48.8566]); // Paris
          var radius = 10; // 10 km
          
          var filtered = x.x.data.filter(function(d) {
            var point = turf.point([d.lon, d.lat]);
            return turf.distance(center, point, {units: 'kilometers'}) <= radius;
          });

          console.log('Points après filtrage:', filtered.length);
        };
        document.head.appendChild(script);
      }
    ")
}


