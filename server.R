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
  
  base_url <- "https://api.jcdecaux.com/vls/v3/stations"
  url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)
  
  # Création du dataframe, à la mise en ligne utiliser la base de données
  VelovList <- fromJSON(rawToChar(GET(url)$content))

  
  ## DEBLOQUER au démarrage
  #VelovList$adresse<-reverse_geo(lat = VelovList$position$lat, long = VelovList$position$lng, method = "osm")
  
  # Récupération des données pour les KPI
  ### 1 
  placeTotal = mean(VelovList$totalStands$availabilities$stands)
  nombreVeloDispo =  mean(VelovList$totalStands$availabilities$bikes)
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
  
  veloDispo = sum(VelovList$totalStands$availabilities$bikes)
  
  output$veloDispo_box <- renderText({
    paste0(veloDispo)  # Affichez la variable pourcentage
  })
  
  
  ### La carte 
  observe({
  
    num_stations <- input$nombre_stations
    
    # Filtrer les données en fonction du nombre de stations sélectionné
    VelovList_filtered <- VelovList[1:num_stations, ]
    
    #VelovList$adresse<-reverse_geo(lat = VelovList$position$lat, long = VelovList$position$lng, method = "osm")
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        ## a voir si carte marche pas, changer lng et lat
        setView(lng = mean(VelovList_filtered$position$longitude), lat = mean(VelovList_filtered$position$latitude), zoom = 13) %>%
        addMarkers(data = VelovList_filtered, ~position$longitude, ~position$latitude, label = ~adresse, popup = ~adresse)
    })
  })
  ##### Page info station
  
  observeEvent(input$bouton_recherche, {
    recherche <- input$recherche
    
    # Effectuez la recherche dans le dataframe VelovList
    resultat_recherche <- VelovList[grepl(recherche, VelovList$name, ignore.case = TRUE), ]
    
    # Appelle API pour les stations
    ##//api.jcdecaux.com/vls/v3/stations/{station_number}?contract={contract_name} HTTP/1.1
    
    # Mettez à jour le texte affiché avec les résultats de la recherche
    output$resultat_recherche <- renderText({
      if (nrow(resultat_recherche) > 0) {
        # Afficher les résultats de la recherche
        paste("Résultats de la recherche :")
        # Vous pouvez personnaliser l'affichage ici
        # On trouve la ligne de la station dans le df avec le numéro qui correspond au nom chercher
        stationVelo = VelovList[VelovList$number == resultat_recherche$number, ]
        # Afficher tout les détails de la station
        stationVelo$totalStands$availabilities$electricalBikes
      } else {
        "Aucun résultat trouvé"
      }
    })
  })
}