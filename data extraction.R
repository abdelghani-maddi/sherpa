# Analyse data sherpa romeo
rm(list=ls())
# Charger les bibliothèques
library(httr)
library(jsonlite)
library(readxl)
library(tidyverse)
library(questionr)  

## Données Revues : fichier avec 253222 lignes contenant des ISSN/EISSN de revues extrait à partir de OpenAlex. Soit 186347 supports (y compris revues) distincts.
journals_openalex <- read_excel("~/Documents/bdd pubpeer/journals_openalex.xlsx")

## Données (E)ISSN depuis DOAJ
doaj <- read.csv("D:/Sherpa/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")
doaj_e_issn <- data.frame(c(doaj$Journal.ISSN..print.version., doaj$Journal.EISSN..online.version.))
names(doaj_e_issn) <- "issn"
doaj_e_issn <- doaj_e_issn %>%
  filter(issn != "") %>%
  unique()

# Créer une liste pour stocker les données extraites depuis Sherpa.
data_list <- list()

# Créer un vecteur pour stocker les ISSN ayant des données valides.
valid_issn <- c()

# Spécification du point de départ (l'index à partir duquel la requête reprendra en cas d'interruption)
point_depart <- 1

# Boucle pour extraire les données

# Boucle avec les données OpenAlex
#for (i in point_depart:length(journals_openalex$issn)) {
#  issn <- journals_openalex$issn[i]
  
# Boucle avec les données ISSN
for (i in point_depart:length(doaj_e_issn$issn)) {
  issn <- doaj_e_issn$issn[i]
    
    tryCatch({
    # Construire l'URL avec l'ISSN actuel.
    url <- paste0("https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=", issn)
    
    # Effectuer la requête GET pour récupérer les données JSON.
    response <- GET(url)
    
    # Parser les données JSON.
    json_data <- fromJSON(content(response, "text", encoding = "ISO-8859-1"))
    
    # Vérifier si les données JSON ne sont pas vides et si le champ "items" contient des données.
    if (!is.null(json_data) && !is.null(json_data[["items"]]) && length(json_data[["items"]]) > 0) {
      data_list <- append(data_list, list(json_data))
      valid_issn <- c(valid_issn, issn)
    } else {
      cat(paste("Les données sont vides pour ISSN", issn, "\n"))
    }
  }, error = function(e) {
    cat("Erreur lors de la requête pour ISSN", issn, ":", conditionMessage(e), "\n")
  })
  
  # Sauvegarder le point de départ actuel à chaque itération (l'API rejette la requête et cela permet de relancer autormatiquement à chaque fois)
  point_depart <- i
}

# Sauvegarder data_list dans un fichier RDS : Le fichier extrait de Sherpa contient 43406 revues.
# saveRDS(data_list, file = "data_list.rds") pour les données OpenAlex

saveRDS(data_list, file = "data_list_doaj.rds") # pour les données DOAJ




##############
# Extraire les données sans passer par OpenAlex, en utilisant les filtres de l'API
##############
