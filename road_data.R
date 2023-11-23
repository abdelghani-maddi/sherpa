###################
# Analyse data ROAD
###################

# Nettoyage
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



### Se limiter à 10 records histoire de tester
# Spécifier le chemin du fichier XML
xml_file <- "~/Documents/sherpa_romeo_juliette/ROAD.xml"

# Read the XML file
doc <- read_xml(xml_file)

# Extraire les 10 premiers records
record_nodes <- xml_find_all(doc, "//record[position() <= 63419]")

# Initialiser une liste pour stocker les données
data_list <- list()

# Loop à travers chaque élément record
for (record_node in record_nodes) {
  # Extraire les valeurs des éléments leader, controlfield, datafield
  leader <- xml_text(xml_find_first(record_node, ".//leader"))
  controlfields <- xml_text(xml_find_all(record_node, ".//controlfield"))
  
  # Loop à travers les datafields
  datafields <- list()
  for (datafield_node in xml_find_all(record_node, ".//datafield")) {
    tag <- xml_attr(datafield_node, "tag")
    
    # Extraire les valeurs des subfields
    subfields <- lapply(xml_children(datafield_node), function(subfield_node) {
      code <- xml_attr(subfield_node, "code")
      value <- xml_text(subfield_node)
      paste0(code, "=", value)
    })
    
    # Combinez les tag et subfields dans une chaîne
    datafields <- c(datafields, paste0(tag, ":", paste(subfields, collapse = ";")))
  }
  
  # Combinez toutes les valeurs dans une liste pour chaque enregistrement
  record_data <- c("leader" = leader, controlfields, datafields)
  
  # Ajouter la liste à la liste principale
  data_list <- c(data_list, list(record_data))
}

# Convertir la liste en un dataframe avec rbindlist
df <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

