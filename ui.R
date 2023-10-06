library(shinydashboard)
library(shiny)
library(leaflet)

data <- data.frame(
  Indicator = c("KPI 1", "KPI 2", "KPI 3", "KPI 4", "KPI 5"),
  Value = c(25, 80, 60, 45, 70)
)

# UI de l'application
ui <- dashboardPage(
  dashboardHeader(
    title = "Tableau de bord"
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
            title = "Nombre total de stations de vélos",
            div(style = "text-align: center; font-size: 24px;", textOutput("nbStation_box")),
            width = 4,
            height = 150
          ),
          box(
            # Somme de available_bike
            title = "Nombre total de vélos disponibles",
            div(style = "text-align: center; font-size: 24px;", textOutput("veloDispo_box")),
            width = 4,
            height = 150
          )
        ),
        fluidRow(
          box(
            title = "Filtres",
            selectInput("filtre", "Sélectionnez une option :", choices = c("Option 1", "Option 2", "Option 3")),
            width = 4,
            height = 600
          ),
          # Carte en bas à droite en grand (exclu de l'onglet "Utilisateurs")
          
          box(
            title = "Carte",
            div(style = "height: 100%;", leafletOutput("ma_carte")),
            width = 8, # Largeur réduite pour faire de la place aux indicateurs
            height = 600 # Ajustez la hauteur en fonction de vos besoins
          
          )
          
        )
      ),
      tabItem(tabName = "info_station",
              # Contenu de l'onglet "Info station" (à ajouter)
              # Vous pouvez placer ici les informations spécifiques aux stations
      ),
      tabItem(tabName = "utilisateurs",
              # Contenu de l'onglet "Utilisateurs" (à ajouter)
              # Vous pouvez placer ici les informations relatives aux utilisateurs
      )
    )
  )
)