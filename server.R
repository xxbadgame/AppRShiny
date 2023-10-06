library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(leaflet)

server <- function(input, output) {
  
  # Initialisation de l'API
  contract <- "Lyon"
  api_key <- "adbb8b83872ee11f750777848be5ccd202789a01" 
  
  base_url <- "https://api.jcdecaux.com/vls/v1/stations"
  url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)
  
  # Création du dataframe, à la mise en ligne utiliser la base de données
  VelovList <- fromJSON(rawToChar(GET(url)$content))
  
  # Récupération des données pour les KPI
  ### 1 
  placeTotal = mean(VelovList$bike_stands)
  nombreVeloDispo =  mean(VelovList$available_bikes)
  tauxVeloDispoParStation = round(nombreVeloDispo/placeTotal, digits = 3)
  pourcentage = paste0(tauxVeloDispoParStation * 100 , " %")
  
  output$pourcentage_box <- renderText({
    paste0(pourcentage)  # Affichez la variable pourcentage
  })
  
  ### 2, nombre total de station de velo
  
  nombreStation = nrow(VelovList)
  
  output$nbStation_box <- renderText({
    nombreStation  # Affichez la variable pourcentage
  })
  
  ### 3, nombre total de velo disponible
  
  veloDispo = sum(VelovList$available_bikes)
  
  output$veloDispo_box <- renderText({
    paste0(veloDispo)  # Affichez la variable pourcentage
  })
  
  ### filtre
  
  output$texte <- renderText({
    paste("Vous avez sélectionné :", input$filtre)
  })
  
  ### La carte 
  
  #VelovList$adresse<-reverse_geo(lat = VelovList$position$lat, long = VelovList$position$lng, method = "osm")
  
  output$ma_carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Ajoutez les tuiles de fond (carte de base)
      addMarkers(
        lng = 2.3522,  # Longitude
        lat = 48.8566,  # Latitude
        #popup = "Lyon"  # Popup au clic sur le marqueur on peut ajouter des détails
      )
  })
  
  
}