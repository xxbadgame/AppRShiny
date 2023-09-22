library(shiny)
library(shinydashboard)

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
          # Indicateurs 1, 2 et 3 côte à côte (exclu de l'onglet "Utilisateurs")
          box(
            #Calculez la moyenne du nombre de vélos disponibles par rapport au nombre total de supports à vélos dans toutes les stations.
            #Formule : (Somme des "available_bikes") / (Somme des "bike_stands")
            title = "Taux d'occupation moyen des stations de vélos",
            valueBoxOutput("kpi1_box"),
            width = 4,
            height = 150
          ),
          box(
            # Taille de la base de donnée
            title = "Nombre total de stations de vélos",
            valueBoxOutput("kpi2_box"),
            width = 4,
            height = 150
          ),
          box(
            # Somme de available_bike
            title = "Nombre total de vélos disponibles",
            valueBoxOutput("kpi3_box"),
            width = 4,
            height = 150
          )
        ),
        fluidRow(
            box(
              title = "Filtres",
              valueBoxOutput("kpi4_box"),
              width = 4,
              height = 600
            ),
          # Carte en bas à droite en grand (exclu de l'onglet "Utilisateurs")
          
          box(
            title = "Carte",
            width = 8, # Largeur réduite pour faire de la place aux indicateurs
            height = 600, # Ajustez la hauteur en fonction de vos besoins
            plotOutput("map")
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

# Serveur de l'application
server <- function(input, output) {
  # Fonction pour afficher les KPI
  renderValueBox({
    indicator_name <- data$Indicator[which(data$Indicator == output$id)]
    value <- data$Value[which(data$Indicator == output$id)]
    valueBox(
      value = value,
      subtitle = indicator_name,
      color = "blue"
    )
  })
  
  # Placeholder pour la carte (à ajouter)
  output$map <- renderPlot({
    # Ajoutez ici votre code pour afficher une carte
    # Utilisez leaflet ou un autre package de cartographie pour créer la carte
    # Assurez-vous d'ajuster les paramètres et les données en fonction de votre besoin
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
