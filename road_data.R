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


# Spécifier le chemin du fichier XML
xml_file <- "~/Documents/sherpa_romeo_juliette/ROAD.xml"

# Lire le fichier XML
doc <- read_xml(xml_file)

# Extraire tous les éléments "record"
record_nodes <- xml_find_all(doc, "//record")

# Initialiser une liste pour stocker les données
data_list <- list()

# Loop à travers chaque élément record
for (record_node in record_nodes) {
  # Extraire les valeurs des éléments leader, controlfield, datafield
  leader <- xml_text(xml_find_first(record_node, ".//leader"))
  controlfields <- xml_text(xml_find_all(record_node, ".//controlfield"))
  datafields <- xml_text(xml_find_all(record_node, ".//datafield"))
  
  # Combinez toutes les valeurs dans une liste pour chaque enregistrement
  record_data <- c("leader" = leader, "controlfields" = controlfields, "datafields" = datafields)
  
  # Ajouter la liste à la liste principale
  data_list <- c(data_list, list(record_data))
}

# Convertir la liste en dataframe
df <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)
