# Analyse data sherpa romeo
rm(list=ls())
# Charger les bibliothèques
library(httr)
library(jsonlite)
library(readxl)
library(tidyverse)
library(questionr)  

## Données Revues : fichier avec 253222 lignes contenant des ISSN/EISSN de revues extrait à partir de OpenAlex. Soit 186347 supports (y compris revues) distincts.
#  journals_openalex <- read_excel("~/Documents/bdd pubpeer/journals_openalex.xlsx")

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
copyright_owner_phrases_concatenated <- character(43406)
article_version_concatenated <- character(43406)
license_concatenated <- character(43406)
location_phrases_concatenated <- character(43406)
named_repository_concatenated <- character(43406)

# Boucle pour extraire et concaténer les valeurs
for (i in 1:43406) {
  item <- revues_sherpa[[i]][["items"]]
  
  # additional_oa_fee
  additional_oa_fee_values <- item[["publisher_policy"]][[1]][["permitted_oa"]]
  
  if (length(additional_oa_fee_values) > 0) {
    concatenated_values <- sapply(additional_oa_fee_values, function(x) paste(x[["additional_oa_fee"]], collapse = " - "))
    additional_oa_fee_concatenated[i] <- paste(concatenated_values, collapse = " | ")
  } else {
    additional_oa_fee_concatenated[i] <- NA
  }
  
  
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
  
  
  # permitted oa
  permitted_oa_values <- item[["publisher_policy"]][[1]][["permitted_oa"]]
  
  if (length(permitted_oa_values) > 0) {
    concatenated_values <- sapply(permitted_oa_values, function(x) paste(x$article_version, collapse = " - "))
    article_version_concatenated[i] <- paste(concatenated_values, collapse = " | ")
  } else {
    article_version_concatenated[i] <- NA
  }
  
  
  # Licence
  license <- item[["publisher_policy"]][[1]][["permitted_oa"]][[1]][["license"]]
  license_concatenated[i] <- if (!is.null(license)) paste(license, collapse = " - ") else NA

  # copyright_owner_phrases
  copyright_owner_phrases <- item[["publisher_policy"]][[1]][["permitted_oa"]][[1]][["copyright_owner_phrases"]]
  copyright_owner_phrases_concatenated[i] <- if (!is.null(copyright_owner_phrases)) paste(copyright_owner_phrases, collapse = " - ") else NA
  
  # location_phrases
  location_phrases <- item[["publisher_policy"]][[1]][["permitted_oa"]][[1]][["location"]][["location_phrases"]][[1]]
  location_phrases_concatenated[i] <- if (!is.null(location_phrases)) paste(location_phrases, collapse = " - ") else NA
  
  # Licence
  named_repository <- item[["publisher_policy"]][[1]][["permitted_oa"]][[1]][["location"]][["named_repository"]]
  named_repository_concatenated[i] <- if (!is.null(named_repository)) paste(named_repository, collapse = " - ") else NA
  
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
  publisher_country = publisher_country_concatenated,
  copyright_owner_phrases = copyright_owner_phrases_concatenated,
  article_version = article_version_concatenated,
  license = license_concatenated,
  location_phrases = location_phrases_concatenated,
  named_repository = named_repository_concatenated
)


# explo types
type <- freq(df$relationship_type)


# Fonction pour concaténer les valeurs d'un champ
concatenate_field <- function(item, field_name) {
  if (field_name == "listed_in_doaj") {
    field_values <- item[[field_name]]
  } else if (field_name == "issn") {
    field_values <- item[["issns"]][[1]][[field_name]]
  } else if (field_name == "country") {  # Ajustement ici
    field_values <- item[["publishers"]][[1]][["publisher"]]
    if (!is.null(field_values) && !is.null(field_values[[field_name]])) {
      return(paste(field_values[[field_name]], collapse = " - "))
    } else {
      return(NA)
    }
  } else if (field_name %in% c("relationship_type")) {
    field_values <- item[["publishers"]][[1]][[field_name]]
  } else {
    field_values <- item[["publisher_policy"]][[1]][["permitted_oa"]]
  }
  
  if (!is.null(field_values)) {
    if (field_name %in% c("listed_in_doaj", "issn", "relationship_type")) {
      return(paste(field_values, collapse = " - "))
    } else {
      concatenated_values <- sapply(field_values, function(x) {
        if (!is.null(x[[field_name]])) {
          return(x[[field_name]])
        } else {
          return(NA)
        }
      })
      return(paste(concatenated_values, collapse = " | "))
    }
  } else {
    return(NA)
  }
}


# Créer des vecteurs pour stocker les valeurs concaténées
fields <- c(
  "additional_oa_fee",
  "listed_in_doaj",
  "issn",
  "license",
  "location",
  "copyright_owner",
  "article_version",
  "embargo",
  "relationship_type",
  "country"
)

concatenated_data <- lapply(fields, function(field) {
  concatenated_values <- character(43406)
  
  for (i in 1:43406) {
    item <- revues_sherpa[[i]][["items"]]
    
    concatenated_values[i] <- concatenate_field(item, field)
  }
  
  return(concatenated_values)
})

# Créer un dataframe avec les valeurs concaténées
df <- data.frame(setNames(concatenated_data, fields))












