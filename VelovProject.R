library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)

contract <- "Lyon"
api_key <- "2aa23a6e7a71f92dc1a40fa2d412650e19b794f1" 

base_url <- "https://api.jcdecaux.com/vls/v1/stations"
url <- paste0(base_url, "?contract=", contract, "&apiKey=", api_key)

response <- GET(url)

VelovList <- fromJSON(rawToChar(response$content))

# il faut manipuler les df pour avoir les bonnes tables au choix pour notre application

con <- dbConnect(MySQL(),
                  user = "sql11646654",
                  password = "mj1EKmAPJY",
                  host = "sql11.freesqldatabase.com",
                  dbname = "sql11646654")


dbWriteTable(con, "VelovInfo", VelovList)
dbListTables(con)
