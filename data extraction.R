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

# test <- journals_openalex$issn[1:100]

# Créer une liste pour stocker les données extraites depuis Sherpa.
data_list <- list()

# Créer un vecteur pour stocker les ISSN ayant des données valides.
valid_issn <- c()

# Spécification du point de départ (l'index à partir duquel la requête reprendra en cas d'interruption)
point_depart <- 1

# Boucle pour extraire les données
for (i in point_depart:length(journals_openalex$issn)) {
  issn <- journals_openalex$issn[i]
  
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
saveRDS(data_list, file = "data_list.rds")

##########################################
# Charger data_list depuis le fichier RDS
##########################################

revues_sherpa <- readRDS("data_list.rds")


# Créer des vecteurs pour stocker les valeurs concaténées
additional_oa_fee_concatenated <- character(43406)
listed_in_doaj_concatenated <- character(43406)
issn_concatenated <- character(43406)
publication_count_concatenated <- character(43406)
embargo_amount_concatenated <- character(43406)
listed_in_doaj_phrases_concatenated <- character(43406)
relationship_type_concatenated <- character(43406)
publisher_country_concatenated <- character(43406)

# Boucle pour extraire et concaténer les valeurs
for (i in 1:43406) {
  item <- revues_sherpa[[i]][["items"]]
  
  # Champ "additional_oa_fee"
  additional_oa_fee_values <- item[["publisher_policy"]][[1]][["permitted_oa"]][[1]][["additional_oa_fee"]]
  additional_oa_fee_concatenated[i] <- if (!is.null(additional_oa_fee_values)) paste(additional_oa_fee_values, collapse = " - ") else NA
  
  # Champ "listed_in_doaj"
  listed_in_doaj_values <- item[["listed_in_doaj"]]
  listed_in_doaj_concatenated[i] <- if (!is.null(listed_in_doaj_values)) paste(listed_in_doaj_values, collapse = " - ") else NA
  
  # Champ "issn"
  issn_values <- item[["issns"]][[1]][["issn"]]
  issn_concatenated[i] <- if (!is.null(issn_values)) paste(issn_values, collapse = " - ") else NA
  
  # Champ "publication_count"
  publication_count_values <- item[["publisher_policy"]][[1]][["publication_count"]]
  publication_count_concatenated[i] <- if (!is.null(publication_count_values)) paste(publication_count_values, collapse = " - ") else NA
  
  # Champ "embargo_amount"
  embargo_amount_values <- item[["publisher_policy"]][[1]][["permitted_oa"]][[1]][["embargo"]][["amount"]]
  embargo_amount_concatenated[i] <- if (!is.null(embargo_amount_values)) paste(embargo_amount_values, collapse = " - ") else NA
  
  # Champ "listed_in_doaj_phrases"
  listed_in_doaj_phrases_values <- item[["listed_in_doaj_phrases"]][[1]][["value"]]
  listed_in_doaj_phrases_concatenated[i] <- if (!is.null(listed_in_doaj_phrases_values)) paste(listed_in_doaj_phrases_values, collapse = " - ") else NA
  
  # Champ "relationship_type"
  relationship_type_values <- item[["publishers"]][[1]][["relationship_type"]]
  relationship_type_concatenated[i] <- if (!is.null(relationship_type_values)) paste(relationship_type_values, collapse = " - ") else NA
  
  # Champ "publisher_country"
  publisher_country_values <- item[["publishers"]][[1]][["publisher"]][["country"]]
  publisher_country_concatenated[i] <- if (!is.null(publisher_country_values)) paste(publisher_country_values, collapse = " - ") else NA
}

# Créer un dataframe avec les valeurs concaténées
df <- data.frame(
  additional_oa_fee = additional_oa_fee_concatenated,
  listed_in_doaj = listed_in_doaj_concatenated,
  issn = issn_concatenated,
  publication_count = publication_count_concatenated,
  embargo_amount = embargo_amount_concatenated,
  listed_in_doaj_phrases = listed_in_doaj_phrases_concatenated,
  relationship_type = relationship_type_concatenated,
  publisher_country = publisher_country_concatenated
)


# explo types
type <- freq(df$relationship_type)






