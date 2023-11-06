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

