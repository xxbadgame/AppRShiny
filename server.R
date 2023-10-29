library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(leaflet)
library(readr)
library(shinyjs)
library(ggplot2)


#con <- dbConnect(MySQL(),
#                  user = "sql11646654",
#                  password = "mj1EKmAPJY",
#                  host = "sql11.freesqldatabase.com",
#                  dbname = "sql11646654")


#dbWriteTable(con, "VelovInfo", VelovList)
#dbListTables(con)

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
  
  # Calculez la somme des vélos mécaniques et électriques
  sum_mechanical <- sum(VelovList$totalStands$availabilities$mechanicalBikes)
  sum_electrical <- sum(VelovList$totalStands$availabilities$electricalBikes)
  
  # Calculez les pourcentages
  total_bikes <- sum_mechanical + sum_electrical
  percent_mechanical <- (sum_mechanical / total_bikes) * 100
  percent_electrical <- (sum_electrical / total_bikes) * 100
  
  # Créez un graphique circulaire (pie chart)
  output$pie_chart <- renderPlot({
    data <- data.frame(
      type = c("Mécaniques", "Électriques"),
      value = c(percent_mechanical, percent_electrical)
    )
    
    mon_graphique <<- ggplot(data, aes(x = "", y = value, fill = type)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = sprintf("%1.1f%%", value)), position = position_stack(vjust = 0.5)) + # Ajout des pourcentages
      coord_polar(theta = "y") +
      labs(title = "Répartition des Vélos Mécaniques et Électriques",
           x = NULL, y = NULL) +
      theme_void() +
      scale_fill_manual(values = c("Mécaniques" = "blue", "Électriques" = "gray"))
    
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
    })
}
