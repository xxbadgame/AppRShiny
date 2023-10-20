library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(leaflet)
library(readr)
library(shinyjs)

server <- function(input, output) {
  
  # Initialisation de l'API
  contract <- "Lyon"
  api_key <- "adbb8b83872ee11f750777848be5ccd202789a01" 
  
  base_url <- "https://api.jcdecaux.com/vls/v3/stations"
  url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)
  
  # Création du dataframe, à la mise en ligne utiliser la base de données
  VelovList <- fromJSON(rawToChar(GET(url)$content))
  
  
  ## DEBLOQUER au démarrage
  #VelovList$adresse<-reverse_geo(lat = VelovList$position$latitude, long = VelovList$position$longitude, method = "osm")
  VelovList$adresse <- read.csv("VelovAdresses.csv")
  
  
  # Récupération des données pour les KPI
  
  ## Bouton rafrachir
  
  observeEvent(input$bouton_refresh, {
    
    
    # Afficher l'animation de chargement au début
    shinyjs::show("loading_animation")
    
    # Exécuter la commande (par exemple, VelovList <- fromJSON(rawToChar(GET(url)$content))
    VelovList <- fromJSON(rawToChar(GET(url)$content))
    
    # Masquer l'animation de chargement après le traitement
    shinyjs::hide("loading_animation")
    
  })
  
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
  
  
  ##### La carte ######
  observe({
    
    num_stations <- input$nombre_stations
    codep <- input$code_postal
    
    # Filtrer les données en fonction du nombre de stations sélectionné
    VelovList_filtered <- VelovAdresses[1:num_stations, ]
    
    # Filtrer les données en fonction du code postal saisi
    if (codep != "") {
      VelovList_filtered <- VelovAdresses[VelovAdresses$cp == codep, ]
    }
    
    # Créer une carte Leaflet avec les stations filtrées
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addTiles()
      
      for (i in 1:nrow(VelovList_filtered)) {
        m <- m %>%
          addMarkers(
            lng = VelovList_filtered[i, "long"],
            lat = VelovList_filtered[i, "lat"],
            popup = paste("Adresse: ", VelovList_filtered[i, "address"], "<br>",
                          "Vélos disponibles: ", VelovList_filtered[i, "velodispo"], "<br>",
                          "Places parking total: ", VelovList_filtered[i, "placetot"], "<br>",
                          "Places disponibles: ", VelovList_filtered[i, "placedispo"])
          )
      }
      m
    })
    
  })
  ##### Page info station ######
  
  ####### INTEGRER LA SOLUTION D'IMADE, J'AI DEJA MIS LE FRONT AVEC LES BOX MAIS JE N'AI PAS REUSSI POUR LE BACK #####
  
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
        paste("Electric bike : ",stationVelo$totalStands$availabilities$electricalBikes)
      } else {
        "Aucun résultat trouvé"
      }
    })
  })
} 