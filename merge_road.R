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
library(readxl)
library(readr)

# Chargez la liste
# data_list <- readRDS("road_data.rds")

# importer les données ROAD
road_data <- read_excel("road_data.xlsx")
###################
###################
# Revues OpenAlex
###################

openalex <- read_excel("~/Documents/sherpa_romeo_juliette/journals_openalex.xlsx")

###################
# Revues DOAJ
###################
doaj <- read_csv("~/Documents/sherpa_romeo_juliette/journalcsv__doaj_20231110_1320_utf8.csv")

doaj <- doaj %>%
  select(`Journal ISSN (print version)`, `Journal EISSN (online version)`)

names(doaj) <- c("issn", "eissn")

doaj <- c(doaj$issn, doaj$eissn) %>%
  as.data.frame() %>%
  unique()
names(doaj) <- "doaj"
doaj <- doaj %>%
  filter(!is.na(.$doaj))


###################
# Revues Scopus
###################

scopus <- read_excel("~/Documents/sherpa_romeo_juliette/ext_list_November_2023_corrections.xlsx", 
                     sheet = "Scopus Sources October 2023")

scopus <- scopus %>%
  filter(`Active or Inactive` == "Active") %>%
  select(`Print-ISSN`, `E-ISSN`) %>%
  unique()

names(scopus) <- c("issn", "eissn")

scopus <- c(scopus$issn, scopus$eissn) %>%
  as.data.frame() %>%
  unique()
names(scopus) <- "scopus1"
scopus <- scopus %>%
  filter(!is.na(.$scopus1))
# mettre en forme les issn
scopus <- scopus %>%
  mutate(scopus = paste0(str_sub(scopus1, 1, 4), "-", str_sub(scopus1, 5))) %>%
  select(-scopus1)

###################
# Revues Sherpa Romeo
###################

sherpa <- read_excel("~/Documents/sherpa_romeo_juliette/df_all_sherpa.xlsx")

###################
# Revues Top Factor
###################
top_factor <- read_csv("~/Documents/sherpa_romeo_juliette/top-factor.csv")

###################
# Revues WoS
###################
wos_SCIE <- read_csv("~/Documents/sherpa_romeo_juliette/wos-core_SCIE 2023-November-20.csv")
wos_AHCI <- read_csv("~/Documents/sherpa_romeo_juliette/wos-core_AHCI 2023-November-20.csv")
wos_SSCI <- read_csv("~/Documents/sherpa_romeo_juliette/wos-core_SSCI 2023-November-20.csv")
wos_ESCI <- read_csv("~/Documents/sherpa_romeo_juliette/wos-core_ESCI 2023-November-20.csv")

wos <- bind_rows(wos_SCIE, wos_AHCI, wos_SSCI, wos_ESCI)

###################
# CoalitionS DOAJ
###################
coalitions_doaj <- read_excel("~/Documents/sherpa_romeo_juliette/crowdsourced_list_of_diamond_journals.xlsx", 
                                                    sheet = "0) DIAMOND JOURNALS - DOAJ")
###################
# CoalitionS NON DOAJ (que 64 revues)
###################

coalitions_non_doaj <- read_excel("~/Documents/sherpa_romeo_juliette/crowdsourced_list_of_diamond_journals.xlsx", 
                                  sheet = "1) DIAMOND JOURNALS - NOT IN DO")



###################################################
###################################################
# Matching
###################################################
###################################################