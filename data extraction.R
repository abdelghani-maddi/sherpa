# Charger les bibliothèques
library(httr)
library(jsonlite)
library(readxl)

## Données Revues

journals_openalex <- read_excel("~/Documents/bdd pubpeer/journals_openalex.xlsx")

journals_openalex$issn[1:10]


url_cancer <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=cancer"

url_anr <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=funder&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=30"
url <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=1431-5890"

# URL de l'API
url <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=1097-0142"
# Effectuer la requête GET pour récupérer les données JSON
response <- GET(url_anr)

# Parser les données JSON directement
json_data <- fromJSON(content(response, "text", encoding = "ISO-8859-1"))


# Créer une liste pour stocker les données extraites.
data_list <- list()

# Parcourez la colonne "issn" du dataframe.
for (issn in journals_openalex$issn[1:100]) {
  # Construiser l'URL avec l'ISSN actuel.
  url <- paste0("https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=", issn)
  
  # Effectuez la requête GET pour récupérer les données JSON.
  response <- GET(url)
  # Parsez les données JSON et ajoutez-les à la liste.
  json_data <- fromJSON(content(response, "text", encoding = "ISO-8859-1"))
  data_list <- append(data_list, list(json_data))
}

# La liste data_list contient maintenant les données extraites pour tous les ISSN de votre dataframe.




