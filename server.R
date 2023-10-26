library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(leaflet)
library(readr)
library(shinyjs)

server <- function(input, output) {
  observe({
    # Vérifier si le bouton de connexion a été cliqué
    if (input$loginButton > 0) {
      # Récupérer le nom d'utilisateur saisi
      username <- input$username
      # Récupérer le mot de passe saisi
      password <- input$password
      
      # Vérifier que les champs d'identifiants ne sont pas vides
      if (!is.null(username) && !is.null(password)) {
        # Ici, vous pouvez ajouter des vérifications pour les utilisateurs et les administrateurs
        # Par exemple, vous pouvez vérifier si l'utilisateur est "admin" avec le mot de passe "2022"
        if (username == "admin" && password == "2022") {
          # Afficher un message de bienvenue pour l'administrateur
          shinyjs::alert("Connexion admin réussie")
        } else {
          # Afficher un message de bienvenue pour tous les utilisateurs
          shinyjs::alert("Connexion réussie")
        }
      }
    }
  })
  
  
  
  
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
