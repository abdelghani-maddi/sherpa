# Analyse data sherpa romeo
rm(list=ls())
# Charger les bibliothèques
library(httr)
library(jsonlite)
library(readxl)
library(tidyverse)
library(questionr)
library(openxlsx2)
library(openxlsx)


## Données Revues : fichier avec 253222 lignes contenant des ISSN/EISSN de revues extrait à partir de OpenAlex. Soit 186347 supports (y compris revues) distincts.
journals_openalex <- read_excel("~/Documents/bdd pubpeer/journals_openalex.xlsx")

##
journals_openalex <- read_excel("D:/bdd pubpeer/journals_openalex.xlsx")


journals_doaj <- read.csv("D:/Sherpa/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")
journals_doaj <- read.csv("~/Documents/sherpa_romeo_juliette/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")

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

write.xlsx(df_all_sherpa, "D:/Sherpa/df_all_sherpa.xlsx")

##########################
# Les ISSN DES TROIS BASES
##########################

# DOAJ
doaj <- read.csv("D:/Sherpa/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")
doaj <- read.csv("~/Documents/sherpa_romeo_juliette/journalcsv__doaj_20231110_1320_utf8.csv", sep = ",")

doaj_e_issn <- data.frame(doaj$Journal.ISSN..print.version., doaj$Journal.EISSN..online.version.) %>%
  mutate_all(na_if, "")

colnames(doaj_e_issn)[colnames(doaj_e_issn) == "doaj.Journal.ISSN..print.version."] <- "issn"
colnames(doaj_e_issn)[colnames(doaj_e_issn) == "doaj.Journal.EISSN..online.version."] <- "eissn"

doaj_e_issn$combined_issn_doaj <- paste0(doaj_e_issn$issn, " | " , doaj_e_issn$eissn)
doaj_e_issn$vec_doaj <- str_extract_all(doaj_e_issn$combined_issn_doaj, "\\w{4}-\\w{4}")


# OpenAlex
openalex_e_issn <- journals_openalex %>%
  filter(!is.na(issn)) %>%
  select(id, issn) %>%
  group_by(id) %>%
  summarize(combined_issn_openalex = paste(issn, collapse = " | ")) %>%
  mutate(vec_openalex = str_extract_all(combined_issn_openalex, "\\w{4}-\\w{4}"))

# # Compter le nombre d'ISSN séparés par des barres verticales
# openalex_e_issn_counts <- openalex_e_issn %>%
#   mutate(issn_count = str_count(combined_issn, "\\w{4}-\\w{4}"))

# Sherpa Romeo
sherpa_e_issn <- df_all_sherpa %>%
  select(issn1, issn2)

sherpa_e_issn$combined_issn_sherpa <- paste0(sherpa_e_issn$issn1, " | " , sherpa_e_issn$issn2)
sherpa_e_issn$vec_sherpa <- str_extract_all(sherpa_e_issn$combined_issn_sherpa, "\\w{4}-\\w{4}")

#####################################################################################
#####################################################################################
#####################################################################################

#####################################################################################
# Installer les packages dplyr et tidyr s'ils ne sont pas déjà installés
# install.packages("dplyr")
# install.packages("tidyr")

# Charger les packages
library(dplyr)
library(tidyr)

# Supposons que vous ayez les colonnes "vec_openalex", "vec_sherpa" et "vec_doaj"
# dans les DataFrames openalex_e_issn, sherpa_e_issn et doaj_e_issn respectivement.

# Utiliser tidyr pour étaler les listes
openalex_e_issn <- openalex_e_issn %>%
  unnest(vec_openalex) %>%
  rename(vec = vec_openalex)

sherpa_e_issn <- sherpa_e_issn %>%
  unnest(vec_sherpa) %>%
  rename(vec = vec_sherpa)

doaj_e_issn <- doaj_e_issn %>%
  unnest(vec_doaj) %>%
  rename(vec = vec_doaj)


# Fusionner les DataFrames en utilisant la fonction merge de dplyr avec all = TRUE
match_bdd <- openalex_e_issn %>%
  full_join(sherpa_e_issn, by = "vec") %>%
  full_join(doaj_e_issn, by = "vec") %>%
  select(-vec) %>%
  unique() %>%
  select(starts_with("comb")) %>%
  unique()

names(match_bdd) <- c("OpenAlex", "Sherpa", "DOAJ")

# Ajouter une nouvelle colonne "nb_na" comptant le nombre de NA par ligne
match_bdd$nb_na <- rowSums(is.na(match_bdd))

# Compter le nombre de lignes par "OpenAlex"
nb_lignes_par_openalex <- table(match_bdd$OpenAlex) %>%
  as.data.frame()
# Renommer les colonnes
colnames(nb_lignes_par_openalex) <- c("OpenAlex", "Nb_lignes_openalex")
# Compter le nombre de lignes par "doaj"
nb_lignes_par_doaj <- table(match_bdd$DOAJ) %>%
  as.data.frame()
# Renommer les colonnes
colnames(nb_lignes_par_doaj) <- c("DOAJ", "Nb_lignes_doaj")
# Compter le nombre de lignes par "Sherpa"
nb_lignes_par_Sherpa <- table(match_bdd$Sherpa) %>%
  as.data.frame()
# Renommer les colonnes
colnames(nb_lignes_par_Sherpa) <- c("Sherpa", "Nb_lignes_Sherpa")


# Matcher les nombres de lignes
match_bdd <- left_join(match_bdd, nb_lignes_par_openalex, by = "OpenAlex")
match_bdd <- left_join(match_bdd, nb_lignes_par_doaj, by = "DOAJ")
match_bdd <- left_join(match_bdd, nb_lignes_par_Sherpa, by = "Sherpa")

# Filtrer les doublons openalex
match_bdd <- match_bdd %>%
  filter(!(nb_na = 2 & Nb_lignes_openalex > 1 & !is.na(Nb_lignes_openalex) & is.na(Nb_lignes_doaj) & is.na(Nb_lignes_Sherpa) ))


# Supprimer les lignes pour lesquelles nb_na = 0 (intersection des 3 bdd)
match_bdd_na0 <- match_bdd %>%
  filter(nb_na == 0)

# Sélectionner les lignes de match_bdd où rien ne matche (aucune correspondance entre les 3 bdd)
match_bdd_na2 <- match_bdd %>%
  filter(nb_na == 2 & 
           (Nb_lignes_openalex == 1 & is.na(Nb_lignes_doaj) & is.na(Nb_lignes_Sherpa)) | 
           (Nb_lignes_doaj == 1 & is.na(Nb_lignes_openalex) & is.na(Nb_lignes_Sherpa)) | 
           (Nb_lignes_Sherpa == 1 & is.na(Nb_lignes_doaj) & is.na(Nb_lignes_openalex))) 

# Sélectionner les lignes de match_bdd où il y a une correspondance entre 2 bdd sur 3
match_bdd_na1 <- match_bdd %>%
  filter(nb_na == 1 & 
           (Nb_lignes_openalex == 1 & Nb_lignes_doaj == 1 & is.na(Nb_lignes_Sherpa)) | 
           (Nb_lignes_openalex == 1 & Nb_lignes_Sherpa == 1 & is.na(Nb_lignes_doaj)) | 
           (Nb_lignes_doaj == 1 & Nb_lignes_Sherpa == 1 & is.na(Nb_lignes_openalex))) 

# Sélectionner les lignes de match_bdd où il y a une correspondance entre 2 bdd sur 3
match_bdd_na1_doub <- match_bdd %>%
  filter(nb_na == 1 & 
           (Nb_lignes_openalex > 1 & Nb_lignes_doaj == 1 & is.na(Nb_lignes_Sherpa)) | 
           (Nb_lignes_openalex > 1 & Nb_lignes_Sherpa == 1 & is.na(Nb_lignes_doaj)) | 
           (Nb_lignes_doaj > 1 & Nb_lignes_Sherpa == 1 & is.na(Nb_lignes_openalex))) 

# Sélectionner les lignes de match_bdd où il y a une correspondance entre 2 bdd sur 3
match_bdd_na1_doub2 <- match_bdd %>%
  filter(nb_na == 1 & 
           (Nb_lignes_openalex > 1 & Nb_lignes_doaj > 1 & is.na(Nb_lignes_Sherpa)) | 
           (Nb_lignes_openalex > 1 & Nb_lignes_Sherpa > 1 & is.na(Nb_lignes_doaj)) | 
           (Nb_lignes_doaj > 1 & Nb_lignes_Sherpa > 1 & is.na(Nb_lignes_openalex))) 


# Faire une union des trois dataframes
match_bdd_all <- bind_rows(match_bdd_na0, match_bdd_na1, match_bdd_na2, match_bdd_na1_doub)


n_distinct(match_bdd_all$OpenAlex)
n_distinct(match_bdd_all$Sherpa)
n_distinct(match_bdd_all$DOAJ)

#####################################################################################
# 0996-2743 | 2419-6622 | 0048-8003 | 1243-8804 | 1776-3045 | 2420-4633 | 0996-2743 | 2419-6622 | 0048-8003 | 1243-8804 | 1776-3045 | 2420-4633
#####################################################################################
#####################################################################################
match_bdd2 <- match_bdd_all %>%
  select(-starts_with("n")) %>%
  as.data.frame()

# Transformer en 0/1
match_bdd2$OpenAlex <- ifelse(!is.na(match_bdd2$OpenAlex), 1, 0)
match_bdd2$Sherpa <- ifelse(!is.na(match_bdd2$Sherpa), 1, 0)
match_bdd2$DOAJ <- ifelse(!is.na(match_bdd2$DOAJ), 1, 0)

# tests
sum(match_bdd2$OpenAlex)
sum(match_bdd2$Sherpa)
sum(match_bdd2$DOAJ)


# install.packages("UpSetR")
library(UpSetR)
library(grid)


# Créer le graphique UpSet
upset(match_bdd2, order.by = "freq", nsets = 3, matrix.color = "#DC267F", 
      main.bar.color = "#648FFF", sets.bar.color = "#FE6100",
      point.size = 6,
      text.scale = 2,  # Ajuster la taille des chiffres
      )


