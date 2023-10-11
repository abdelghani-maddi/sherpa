# Charger les bibliothèques
library(httr)
library(jsonlite)

# URL de l'API
url <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=1097-0142"

url_cancer <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=publication&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=cancer"

url_anr <- "https://v2.sherpa.ac.uk/cgi/retrieve_by_id?item-type=funder&api-key=3F5913F6-677E-11EE-8559-EAD6AA1EFE05&format=Json&identifier=30"

# Effectuer la requête GET pour récupérer les données JSON
response <- GET(url_anr)

# Parser les données JSON directement
json_data <- fromJSON(content(response, "text", encoding = "ISO-8859-1"))
