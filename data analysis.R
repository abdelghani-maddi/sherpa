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
  full_join(., sherpa_e_issn, by = "issn") %>%
  unique()

#####################################################################################
#####################################################################################
# Trouver les indices des correspondances pour issn1 et issn2
matching_indices_issn1 <- match(match_bdd$issn_doaj, doaj$Journal.ISSN..print.version.)
matching_indices_issn2 <- match(match_bdd$issn_doaj, doaj$Journal.EISSN..online.version.)

# Combiner les résultats en utilisant l'opérateur |
matching_indices <- ifelse(!is.na(matching_indices_issn1), matching_indices_issn1, matching_indices_issn2)

# Ajouter une colonne id_openalex_result à match_bdd
match_bdd$id_doaj_result <- ifelse(!is.na(matching_indices), doaj$URL.in.DOAJ[matching_indices], NA)


#####################################################################################

#####################################################################################

# Trouver les indices des correspondances pour issn1 (en gérant le cas où issn1 est NA)
matching_indices_issn1 <- ifelse(!is.na(match_bdd$issn_sherpa), match(match_bdd$issn_sherpa, df_all_sherpa$issn1), NA)

# Trouver les indices des correspondances pour issn2 (en gérant le cas où issn2 est NA)
matching_indices_issn2 <- ifelse(!is.na(match_bdd$issn_sherpa), match(match_bdd$issn_sherpa, df_all_sherpa$issn2), NA)

# Combiner les résultats en utilisant l'opérateur |
matching_indices <- ifelse(!is.na(matching_indices_issn1), matching_indices_issn1, matching_indices_issn2)

# Ajouter une colonne id_sherpa_result à match_bdd
match_bdd$id_sherpa_result <- ifelse(!is.na(matching_indices), df_all_sherpa$all_sherpa[matching_indices], NA)


#####################################################################################
#####################################################################################
# Trouver les indices des correspondances
matching_indices <- ifelse(!is.na(match_bdd$issn_openalex), match(match_bdd$issn_openalex, journals_openalex$issn), NA)

# Ajouter une colonne id_openalex_result à match_bdd
match_bdd$id_openalex_result <- ifelse(!is.na(matching_indices), journals_openalex$id[matching_indices], NA)

#####################################################################################
#####################################################################################
match_bdd2 <- match_bdd %>%
  select("issn", starts_with("id_")) %>%
  unique()


filtered_match_bdd <- match_bdd2 %>%
  distinct(id_doaj_result, id_sherpa_result, id_openalex_result, .keep_all = TRUE)


add_non_na_duplicate_count_columns <- function(dataframe, cols_to_check) {
  # Vérifier si le dataframe est vide
  if (nrow(dataframe) == 0) {
    warning("Le dataframe est vide.")
    return(dataframe)
  }
  
  # Initialiser une liste pour stocker les noms des nouvelles colonnes
  new_cols <- paste(cols_to_check, "_non_na_duplicate_count", sep = "_")
  
  # Ajouter les colonnes pour le nombre d'occurrences en dehors des NA
  dataframe[new_cols] <- lapply(dataframe[cols_to_check], function(col) {
    duplicated_count <- duplicated(col) + duplicated(col, fromLast = TRUE)
    duplicated_count[duplicated_count > 1] <- duplicated_count[duplicated_count > 1] + 1
    duplicated_count[is.na(col)] <- NA
    return(duplicated_count)
  })
  
  return(dataframe)
}

# Colonnes à considérer pour les doublons
cols_to_check <- c("id_doaj_result", "id_sherpa_result", "id_openalex_result")

# Exemple d'utilisation
filtered_match_bdd <- add_non_na_duplicate_count_columns(filtered_match_bdd, cols_to_check)









# Transformer en 0/1
match_bdd2$id_doaj_result <- ifelse(!is.na(match_bdd2$id_doaj_result), 1, 0)
match_bdd2$id_openalex_result <- ifelse(!is.na(match_bdd2$id_openalex_result), 1, 0)
match_bdd2$id_sherpa_result <- ifelse(!is.na(match_bdd2$id_sherpa_result), 1, 0)

names(match_bdd2) <- c("issn","DOAJ", "Sherpa Romeo", "OpenAlex")

# Utiliser la fonction table pour compter le nombre de lignes par issn
count_by_issn <- table(match_bdd2$issn) %>%
  data.frame()

# install.packages("UpSetR")
library(UpSetR)
library(grid)


# Créer le graphique UpSet
upset(match_bdd2, order.by = "freq", nsets = 3, matrix.color = "#DC267F", 
      main.bar.color = "#648FFF", sets.bar.color = "#FE6100",
      point.size = 6,
      text.scale = 2,  # Ajuster la taille des chiffres
      )

# Ajouter des étiquettes aux barres horizontales
for (i in seq_along(upset_plot$bar.down$vps)) {
  grid.text(
    label = rowSums(match_bdd[, upset_plot$bar.down$labels[[i]]]),
    x = unit(1, "npc") - unit(0.5, "cm"),
    y = upset_plot$bar.down$vps[[i]]$y,
    just = "left",
    hjust = 1,
    vjust = 0.5,
    gp = gpar(fontsize = 12, fontface = "bold", col = "black")
  )
}



