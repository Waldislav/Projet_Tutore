zoom_dynamique <- function(input, filtered_data) {
  
  return(observeEvent(input$map_zoom, {
    zoom_level <- input$map_zoom
    radius_scale <- pmax(5, 10 / zoom_level) 
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered_data(),
        ~lon, ~lat,
        color = "black",
        opacity = 1,
        weight = 1,
        fillColor = ~ifelse(pfas_sum >= 100, "red", "orange"),
        fillOpacity = 1,
        radius = radius_scale,
        popup = ~paste(
          "Région:", region, "<br>",
          "Ville:", city, "<br>",
          #"Substance:", substance, "<br>",
          "Somme de pfas:", pfas_sum, "<br>",
          "Année:", year
        )
      ) %>%
      addCircleMarkers(
        data = user,
        ~lon, ~lat,
        color = "grey",
        radius = radius_scale-1,
        fillColor = "grey",
        fillOpacity = 1,
        popup = ~paste("Utilisateur : ", name)
      ) %>%
      addCircleMarkers(
        data = producteur,
        ~lon, ~lat,
        color = "black",
        radius = radius_scale-1,
        fillColor = "black",
        fillOpacity = 1,
        popup = ~paste("Producteur : ", name)
      )
  }))
}