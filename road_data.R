###################
# Analyse data ROAD
# Description des données : https://www.issn.org/services/online-services/road-the-directory-of-open-access-scholarly-resources/ 
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
library(data.table)


### Se limiter à 10 records histoire de tester
# Spécifier le chemin du fichier XML
xml_file <- "~/Documents/sherpa_romeo_juliette/ROAD.xml"

# Read the XML file
doc <- read_xml(xml_file)

#################
# Extraire les 10 premiers records
record_nodes <- xml_find_all(doc, "//record[position() <= 63419]")

# Initialiser une liste pour stocker les données
data_list <- list()

# Loop à travers chaque élément record
for (record_node in record_nodes) {
  # Extraire les valeurs des éléments leader et iso3_code
  leader <- xml_text(xml_find_first(record_node, ".//leader"))
  iso3_code <- xml_text(xml_find_all(record_node, ".//datafield[@tag='044']/subfield[@code='c']"))
  
  # Extraire l'ISSN
  issn_node <- xml_find_first(record_node, ".//controlfield[@tag='001']")
  issn <- ifelse(!is.na(issn_node), xml_text(issn_node), NA)
  
  # Extraire la valeur du champ avec le sous-champ a dans le datafield 981
  field_981_a <- xml_text(xml_find_all(record_node, ".//datafield[@tag='981' and @ind1='0' and @ind2='1']/subfield[@code='a']"))
  
  # Créer une liste pour chaque enregistrement
  record_data <- list(leader = leader, iso3_code = iso3_code, issn = issn)
  
  # Ajouter field_981_a à la liste uniquement s'il est disponible
  if (length(field_981_a) > 0) {
    record_data$field_981_a <- field_981_a
  }
  
  # Ajouter la liste à la liste principale
  data_list <- c(data_list, list(record_data))
}

# Enregistrez la liste dans un fichier
saveRDS(data_list, "road_data.rds")

# Chargez la liste ultérieurement
data_list <- readRDS("road_data.rds")

# Convertir la liste en un dataframe avec rbindlist
df <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

# Utilisez la fonction write.xlsx pour sauvegarder le dataframe en tant que fichier Excel
write.xlsx(df, "road_data.xlsx", row.names = FALSE)


