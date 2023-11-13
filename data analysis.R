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

##
journals_openalex <- read_excel("D:/bdd pubpeer/journals_openalex.xlsx")
journals_openalex <- read_excel("~/Documents/bdd pubpeer/journals_openalex.xlsx")

journals_doaj <- read.csv("D:/Sherpa/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")
journals_doaj <- read.csv("~/Documents/Sherpa/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")

##


##########################################
# Charger data_list depuis le fichier RDS
##########################################

revues_sherpa_openalex <- readRDS("data_list_openalex.rds")
revues_sherpa_doaj <- readRDS("data_list_doaj.rds")
revues_sherpa_all_sherpa <- readRDS("data_list_all_sherpa.rds")


# Fonction pour concaténer les valeurs d'un champ
concatenate_field <- function(item, field_name) {
  if (field_name == "listed_in_doaj") {
    field_values <- item[[field_name]]
  } else if (field_name == "issn") {
    field_values <- item[["issns"]][[1]][[field_name]]
  } else if (field_name == "country") {
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
    
    if (field_name == "named_repository") {
      if (!is.null(field_values)) {
        concatenated_values <- sapply(field_values, function(x) {
          location_values <- x[["location"]]
          if (!is.null(location_values) && !is.null(location_values[["named_repository"]])) {
            return(paste(location_values[["named_repository"]], collapse = " - "))
          } else {
            return(NA)
          }
        })
        return(paste(concatenated_values, collapse = " | "))
      } else {
        return(NA)
      }
    }
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
  "named_repository",
  "copyright_owner",
  "article_version",
  "embargo",
  "relationship_type",
  "country"
)


#############################################
############## OpenAlex data ################   
#############################################

concatenated_data <- lapply(fields, function(field) {
  concatenated_values <- character(43406)
  
  for (i in 1:43406) {
    item <- revues_sherpa_openalex[[i]][["items"]]
    
    concatenated_values[i] <- concatenate_field(item, field)
  }
  
  return(concatenated_values)
})

# Créer un dataframe avec les valeurs concaténées
df_openalex <- data.frame(setNames(concatenated_data, fields))
# Diviser la colonne "issn" en deux parties au niveau du "-"
df_openalex <- separate(df_openalex, issn, into = c("issn1", "issn2"), sep = " - ") %>%
  mutate(id_openalex = 1:length(df_openalex$additional_oa_fee))


#############################################
################ doaj data ##################   
#############################################

concatenated_data <- lapply(fields, function(field) {
  concatenated_values <- character(9838)
  
  for (i in 1:9838) {
    item <- revues_sherpa_doaj[[i]][["items"]]
    
    concatenated_values[i] <- concatenate_field(item, field)
  }
  
  return(concatenated_values)
})

# Créer un dataframe avec les valeurs concaténées
df_doaj <- data.frame(setNames(concatenated_data, fields))
# Diviser la colonne "issn" en deux parties au niveau du "-"
df_doaj <- separate(df_doaj, issn, into = c("issn1", "issn2"), sep = " - ") %>%
  mutate(id_doaj = 1:length(df_doaj$additional_oa_fee))



#############################################
############# all sherpa data ###############   
#############################################

concatenated_data <- lapply(fields, function(field) {
  concatenated_values <- character(27572)
  
  for (i in 1:27572) {
    item <- revues_sherpa_all_sherpa[[i]][["items"]]
    
    concatenated_values[i] <- concatenate_field(item, field)
  }
  
  return(concatenated_values)
})

# Créer un dataframe avec les valeurs concaténées
df_all_sherpa <- data.frame(setNames(concatenated_data, fields))
# Diviser la colonne "issn" en deux parties au niveau du "-"
df_all_sherpa <- separate(df_all_sherpa, issn, into = c("issn1", "issn2"), sep = " - ") %>%
  mutate(all_sherpa = 1:length(df_all_sherpa$additional_oa_fee))


##########################
# Les ISSN DES TROIS BASES
##########################

# DOAJ
doaj <- read.csv("D:/Sherpa/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")
doaj <- read.csv("~/Documents/sherpa_romeo_juliette/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")

doaj_e_issn <- data.frame(c(doaj$Journal.ISSN..print.version., doaj$Journal.EISSN..online.version.))
names(doaj_e_issn) <- "issn"
doaj_e_issn <- doaj_e_issn %>%
  filter(issn != "") %>%
  unique() %>%
  mutate(issn_doaj = issn)

# OpenAlex
openalex_e_issn <- journals_openalex %>%
  select(issn) %>%
  unique() %>%
  mutate(issn_openalex = issn)



# Sherpa Romeo
sherpa_e_issn <- df_all_sherpa %>%
  select(issn1, issn2)
sherpa_e_issn <- data.frame(c(sherpa_e_issn$issn1, sherpa_e_issn$issn2)) %>%
  unique()
names(sherpa_e_issn) <- "issn"

sherpa_e_issn <- sherpa_e_issn %>% mutate(issn_sherpa = issn) 


# Utiliser full_join pour obtenir toutes les lignes
match_bdd <- full_join(doaj_e_issn, openalex_e_issn, by = "issn") %>%
  full_join(., sherpa_e_issn, by = "issn") 


match_bdd$issn_doaj <- ifelse(!is.na(match_bdd$issn_doaj), 1, 0)
match_bdd$issn_openalex <- ifelse(!is.na(match_bdd$issn_openalex), 1, 0)
match_bdd$issn_sherpa <- ifelse(!is.na(match_bdd$issn_sherpa), 1, 0)

match_bdd <- match_bdd %>%
  select(-issn)

names(match_bdd) <- c("DOAJ", "OpenAlex", "Sherpa Romeo")

# install.packages("UpSetR")
# library(UpSetR)

# Créer le graphique UpSet
upset(match_bdd, order.by = "freq", nsets = 3, matrix.color = "#DC267F", 
      main.bar.color = "#648FFF", sets.bar.color = "#FE6100",
      point.size = 3.5)


# Créer le graphique UpSet : échelle log10
upset(match_bdd, order.by = "freq", nsets = 3, matrix.color = "#DC267F", 
      main.bar.color = "#648FFF", sets.bar.color = "#FE6100",
      scale.sets = "log10", scale.intersections = "log10")

