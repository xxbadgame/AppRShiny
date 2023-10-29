contract <- "Lyon"
api_key <- "adbb8b83872ee11f750777848be5ccd202789a01" 

base_url <- "https://api.jcdecaux.com/vls/v3/stations"
url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)

# Création du dataframe, à la mise en ligne utiliser la base de données
VelovList <- fromJSON(rawToChar(GET(url)$content))


## DEBLOQUER au démarrage
#VelovList$adresse<-reverse_geo(lat = VelovList$position$latitude, long = VelovList$position$longitude, method = "osm")
VelovList$adresse <- read.csv("VelovAdresses.csv")
VelovAdresses$placetot<-VelovList$totalStands
VelovAdresses$velodispo<-VelovList$totalStands$availabilities$bikes
VelovAdresses$placedispo<-VelovList$totalStands$availabilities$stands

# Charger la librairie dplyr si ce n'est pas déjà fait
library(dplyr)

# Créez une nouvelle colonne "cp" dans VelovAdresses en extrayant le "69" suivi de trois chiffres
VelovAdresses <- VelovAdresses %>%
  mutate(cp = regmatches(address, regexpr("\\b69\\d{3}\\b", address)))

