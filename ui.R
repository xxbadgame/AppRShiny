library(shinydashboard)
library(shiny)
library(leaflet)
library(jsonlite)
library(httr)
library(shinyjs)

# Initialisation de l'API
contract <- "Lyon"
api_key <- "adbb8b83872ee11f750777848be5ccd202789a01" 

base_url <- "https://api.jcdecaux.com/vls/v1/stations"
url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)

# Création du dataframe, à la mise en ligne utiliser la base de données
VelovList <- fromJSON(rawToChar(GET(url)$content))


# UI de l'application
ui <- dashboardPage(
  dashboardHeader(
    title = "Velov Board"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carte et indicateurs", tabName = "carte_indicateurs"),
      menuItem("Info station", tabName = "info_station"),
      menuItem("Utilisateurs", tabName = "utilisateurs")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "carte_indicateurs",
        
        
        
        actionButton("bouton_refresh", "Rafraîchir les données",style = "margin-bottom: 10px;"),
        
        
        # Élément pour l'animation de chargement
        useShinyjs(),
        extendShinyjs(text = 'shinyjs.init();', functions = c(
          "init" = "function(params) { Shiny.onInputChange('loading', false); }",
          "show" = "function() { Shiny.onInputChange('loading', true); }",
          "hide" = "function() { Shiny.onInputChange('loading', false); }"
        )),
        div(
          id = "loading_animation",
          class = "loader",
          HTML('<i class="fa fa-spinner fa-spin"></i> Loading...'),
          style = "display: none;" 
        ),
        
        fluidRow(
          box(
            #Taux de velo dispo en moyenne sur les stations
            title = "Taux moyen de vélo en station",
            div(style = "text-align: center; font-size: 24px;", textOutput("pourcentage_box")),
            width = 4,
            height = 150
          ),
          box(
            # Taille de la base de donnée
            title = "Nombre de stations de vélos",
            div(style = "text-align: center; font-size: 24px;", textOutput("nbStation_box")),
            width = 4,
            height = 150
          ),
          box(
            # Somme de available_bike
            title = "Nombre de vélos disponibles",
            div(style = "text-align: center; font-size: 24px;", textOutput("veloDispo_box")),
            width = 4,
            height = 150
          )
        ),
        fluidRow(
          box(
            
            ## Filtre 1 : bouton glissoire pour le nombre de station possible de voir
            title = "Filtres",
            sliderInput("nombre_stations", "Nombre de stations :", min = 1, max = nrow(VelovList), value = 100),
            width = 4,
            height = 200
          ),
          
          ##
          
          # Carte en bas à droite en grand (exclu de l'onglet "Utilisateurs")
          
          box(
            title = "Carte",
            div(style = "height: 100%;", leafletOutput("map")),
            width = 8, # Largeur réduite pour faire de la place aux indicateurs
            height = 600 # Ajustez la hauteur en fonction de vos besoins
            
          )
        )
      ),
      tabItem(tabName = "info_station",
              includeCSS("www/custom.css"),
              fluidRow(
                div(
                  class = "custom-margin",  # Classe CSS pour la marge
                  textInput("recherche", "Chercher une station :", value = "")
                ),
                div(
                  class = "custom-margin",  # Classe CSS pour la marge
                  actionButton("bouton_recherche", "Rechercher")
                ),
                div(
                  class = "custom-margin",  # Classe CSS pour la marge
                  textOutput("resultat_recherche")
                )
              ),
              fluidRow(
                box(
                  # Somme de available_bike
                  title = "Nombre de places disponibles de la station",
                  div(style = "text-align: center; font-size: 24px;", textOutput("placeDispo_box")),
                  width = 4,
                  height = 150
                ),
                box(
                  # Somme de meca available_bike
                  title = "Nombre de vélos mécaniques disponibles",
                  div(style = "text-align: center; font-size: 24px;", textOutput("VeloMecaDispo_box")),
                  width = 4,
                  height = 150
                ),
                box(
                  # Somme de elec available_bike
                  title = "Nombre de vélos éléctriques disponibles",
                  div(style = "text-align: center; font-size: 24px;", textOutput("VeloElecDispo_box")),
                  width = 4,
                  height = 150
                ),
                fluidRow(
                  column(4,
                         hr(),
                         verbatimTextOutput('out6'),
                         selectInput('in6', 'Filtre', VelovList$name, multiple=TRUE, selectize=TRUE),
                        
                ),
                box(
                  title = "Graphique Dynamique",
                  width = 12,
                  plotOutput("graphique_dynamique_station")
                ),
                box(
                  # Bouton d'exportation en PNG
                  downloadButton("exporter_png", "Exporter en PNG"),
                  width = 12,
                )
                ),
                
              ),
      ),
      tabItem(tabName = "utilisateurs",
              h1("Utilisateurs"),
      )     
    ),
    
  )
)

