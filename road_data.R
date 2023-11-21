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
library(XML)
library(xml2)

# Charger les bibliothèques
library(xml2)
library(dplyr)

# Spécifier le chemin du fichier XML
chemin_fichier <- "~/Documents/sherpa_romeo_juliette/ROAD.xml"

# Charger le fichier XML
doc_xml <- read_xml(chemin_fichier)

# Extraire les données des champs datafield
datafields <- xml2::xml_find_all(doc_xml, "//datafield")

# Initialiser une liste pour stocker les données
donnees_liste <- list()

# Parcourir chaque datafield
for (datafield in datafields) {
  # Extraire les valeurs des sous-champs
  tag <- xml2::xml_attr(datafield, "tag")
  ind1 <- xml2::xml_attr(datafield, "ind1")
  ind2 <- xml2::xml_attr(datafield, "ind2")
  
  subfields <- xml2::xml_find_all(datafield, "./subfield")
  subfield_codes <- xml2::xml_attr(subfields, "code")
  subfield_values <- xml2::xml_text(subfields)
  
  # Créer un dataframe pour chaque datafield
  datafield_df <- data.frame(
    tag = tag,
    ind1 = ind1,
    ind2 = ind2,
    subfield_code = subfield_codes,
    subfield_value = subfield_values,
    stringsAsFactors = FALSE
  )
  
  # Ajouter le dataframe à la liste
  donnees_liste[[length(donnees_liste) + 1]] <- datafield_df
}

# Concaténer tous les dataframes dans un seul dataframe
donnees_df <- dplyr::bind_rows(donnees_liste)

# Afficher les premières lignes du dataframe
head(donnees_df)
