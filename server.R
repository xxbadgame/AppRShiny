library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(leaflet)
library(readr)
install.packages("readr")
install.packages("shinyjs")
library(shinyjs)
install.packages("ggplot2")
library(ggplot2)

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
    
    # Filtrer les données en fonction du nombre de stations sélectionné
    VelovList_filtered <- VelovList[1:num_stations, ]
    
    #VelovList$adresse<-reverse_geo(lat = VelovList$position$lat, long = VelovList$position$lng, method = "osm")
    
    ##### RUTH : FAIRE LA BOUCLE, JE N'AI PAS REUSSI A L'INTEGRER. ATTENTION IL FAUT GARDER LA MAP QUI SE FILTRE, NE PAS CHANGER LES NOMS DE VARIABLES ####
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        ## a voir si carte marche pas, changer lng et lat
        setView(lng = mean(VelovList_filtered$position$longitude), lat = mean(VelovList_filtered$position$latitude), zoom = 13) %>%
        #addMarkers(data = VelovList_filtered, ~position$longitude, ~position$latitude, label = ~adresse, popup = ~adresse)
        addMarkers(
          data = VelovList_filtered, ~position$longitude, ~position$latitude,
          label = "Station",
          popup = paste("Adresse: ","<br>",
                        "Vélos disponibles: ","<br>",
                        "Places parking total: ","<br>",
                        "Places disponibles: ","<br>")
        )
    })
  })
  ##### Page info station ######
  
  ####### INTEGRER LA SOLUTION D'IMAD, J'AI DEJA MIS LE FRONT AVEC LES BOX MAIS JE N'AI PAS REUSSI POUR LE BACK #####
  
  observeEvent(input$bouton_recherche, {
    recherche <- input$recherche
    
    # Effectuez la recherche dans le dataframe VelovList
    resultat_recherche <- VelovList[grepl(recherche, VelovList$name, ignore.case = TRUE), ]
    
    # Appelle API pour les stations
    dataStation = GET("https://api.jcdecaux.com/vls/v3/stations/?contract=lyon&apiKey=ef2b5b3f4ce33805dd75a5192dda54ddd11b5b06")
    
    bddStation <- fromJSON ( rawToChar ( dataStation$content ) , flatten =  TRUE )
    df_station = data.frame(bddStation)
    df_station = rbind(bddStation, unlist(bddStation))
    
    # Récupération des données pour les KPI de la deuxieme page
    ### 4
    
    placeTotalDispo = subset(VelovList$totalStands$availabilitie$stands, VelovList$number == recherche)
    
    output$placeDispo_box <- renderText({
      placeTotalDispo
    })
    ### KPI 5
    
    velo_meca_dispo = subset(VelovList$totalStands$availabilitie$mechanicalBikes, VelovList$number == recherche)
    
    output$VeloMecaDispo_box <- renderText({
      velo_meca_dispo
    })
    
    ### KPI 6 
    
    velo_elec_dispo = subset(VelovList$totalStands$availabilitie$electricalBikes, VelovList$number == recherche)
    
    output$VeloElecDispo_box <- renderText({
      velo_elec_dispo
    })
    ### Filtres 
    function(input, output, session) {
      output$out6 <- renderPrint(input$in6)
    }
    donnees_filtrees_reactive <- reactive({
      # Obtenez les stations sélectionnées à partir du filtre (input$in6) comme vecteur de chaînes de caractères
      stations_selectionnees <- input$in6
      
      # Filtrer les données VelovList en fonction des stations sélectionnées
      VelovList[VelovList$name %in% stations_selectionnees, ]
    })
    
    output$graphique_dynamique_station <- renderPlot({
      donnees_filtrees <- donnees_filtrees_reactive()
      
      # Créez un graphique en utilisant ggplot2
      mon_graphique <<- ggplot(data = donnees_filtrees, aes(x = name)) +
        geom_bar(aes(y = totalStands$availabilitie$mechanicalBikes, fill = "Mécaniques"), stat = "identity", position = "dodge") +
        geom_bar(aes(y = totalStands$availabilitie$electricalBikes, fill = "Électriques"), stat = "identity", position = "dodge") +
        labs(title = "Graphique en Barres Dynamique",
             x = "Stations",
             y = "Nombre de Vélos",
             fill = "Type de Vélos") +
        scale_fill_manual(values = c("Mécaniques" = "deepskyblue4", "Électriques" = "gray")) +
        scale_y_continuous(
          limits = c(0, 15),
          breaks = seq(0, 15, by = 1),
          labels = seq(0, 15, by = 1)
        )
      print(mon_graphique)
    })
    
    output$exporter_png <- downloadHandler(
      filename = function() {
        paste("graphique_dynamique.png")
      },
      content = function(file) {
        # Sauvegardez le graphique en tant que fichier PNG
        png(file, width = 800, height = 600)
        print(mon_graphique)  # Remplacez ggplot_graph par le nom de votre objet graphique ggplot2
        dev.off()
      }
    )
    
      
    
    # Mettez à jour le texte affiché avec les résultats de la recherche
    output$resultat_recherche <- renderText({
      if (nrow(resultat_recherche) > 0) {
        # Afficher les résultats de la recherche
        paste("Résultats de la recherche :")
        # Vous pouvez personnaliser l'affichage ici
        # On trouve la ligne de la station dans le df avec le numéro qui correspond au nom chercher
        stationVelo = VelovList[VelovList$number == resultat_recherche$number, ]
        # Afficher tout les détails de la station
        paste()
      } else {
        "Aucun résultat trouvé"
      }
    })
  })
}
