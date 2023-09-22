install.packages(c("httr","jsonlite"))
install.packages("RMySQL")
library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)

#contract <- "Lyon"
api_key <- "2aa23a6e7a71f92dc1a40fa2d412650e19b794f1" 

#base_url <- "https://api.jcdecaux.com/vls/v1/stations"
#url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)

#response <- GET(url)

#VelovList <- fromJSON(rawToChar(response$content))

# il faut manipuler les df pour avoir les bonnes tables au choix pour notre application

con <- dbConnect(MySQL(),
                 user = "sql11646654",
                 password = "mj1EKmAPJY",
                 host = "sql11.freesqldatabase.com",
                 dbname = "sql11646654")


dbWriteTable(con, "VelovInfo", VelovList)
dbListTables(con)

#Création de la table ETATS

dataStation = GET("https://api.jcdecaux.com/vls/v3/stations/?contract=lyon&apiKey=ef2b5b3f4ce33805dd75a5192dda54ddd11b5b06")

bddStation <- fromJSON ( rawToChar ( dataStation$content ) , flatten =  TRUE )
df = data.frame()
df = rbind(bddStation, unlist(bddStation))

dbWriteTable(con, "VelovETATS", df)
dbListTables(con)
