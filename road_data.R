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

# Read the XML file using xmlTreeParse
xml_doc <- xmlTreeParse(xml_file, useInternalNodes = TRUE)

# Extract data from specific nodes
record_nodes <- getNodeSet(xml_doc, "//record")

# Loop through each record node
for (record_node in record_nodes) {
  leader <- xpathSApply(record_node, "//leader", xmlValue)
  controlfield_001 <- xpathSApply(record_node, "//controlfield[@tag='001']", xmlValue)
  
  cat("Leader:", leader, "\n")
  cat("Controlfield 001:", controlfield_001, "\n")
  # Add more lines to print other fields as needed
}
