
library(tidyverse)
library(cartogram)
library(maptools)
data(wrld_simpl)

road <- read_xlsx("road_data_merged.xlsx")

prepcarto <- road %>%
  group_by(iso3_code) %>%
  summarise(across(starts_with("in"), sum))

prepcarto$n <- prepcarto$n*10

wrld_simpl@data$scopus <- prepcarto$in_scopus[match(wrld_simpl@data$ISO3, prepcarto$iso3_code)]
wrld_simpl@data$wos <- prepcarto$in_wos[match(wrld_simpl@data$ISO3, prepcarto$iso3_code)]
wrld_simpl@data$doaj <- prepcarto$in_doaj[match(wrld_simpl@data$ISO3, prepcarto$iso3_code)]
wrld_simpl@data$sherpa <- prepcarto$in_sherpa[match(wrld_simpl@data$ISO3, prepcarto$iso3_code)]
wrld_simpl@data$openalex <- prepcarto$in_openalex[match(wrld_simpl@data$ISO3, prepcarto$iso3_code)]
wrld_simpl@data$topfactor <- prepcarto$in_top_factor[match(wrld_simpl@data$ISO3, prepcarto$iso3_code)]

replace_na_by_zero <- function(x) {
  
  ifelse(is.na(x), 0, x)
  
}

wrld_simpl@data$scopus <- replace_na_by_zero(wrld_simpl@data$scopus)
wrld_simpl@data$wos <- replace_na_by_zero(wrld_simpl@data$wos)
wrld_simpl@data$doaj <- replace_na_by_zero(wrld_simpl@data$doaj)
wrld_simpl@data$sherpa <- replace_na_by_zero(wrld_simpl@data$sherpa)
wrld_simpl@data$openalex <- replace_na_by_zero(wrld_simpl@data$openalex)
wrld_simpl@data$topfactor <- replace_na_by_zero(wrld_simpl@data$topfactor)

wrld_simpl <- spTransform(wrld_simpl[wrld_simpl$scopus > 0,], "+proj=moll") # if spTransform does not work, use sf::st_transform

wrld_dorling <- cartogram_dorling(wrld_simpl, "scopus", m_weight = 0.3)

colrs <- RColorBrewer::brewer.pal(3, "Set2")

svg(paste("plots/cartograms/cartogram_scopus.svg"), width = 8, height = 6)

plot.new()

  plot(wrld_dorling, col= "blue", border ="white", bg = "white")

cartography::layoutLayer(title = "Scopus", 
            sources = "", north = TRUE, tabtitle = TRUE,
            theme = "black.pal", frame = TRUE,  
            author = "ROAD")

cartography::labelLayer(x = wrld_dorling, txt = "ISO2", overlap = FALSE, show.lines = FALSE, 
          halo = TRUE, r = .15)

dev.off()


######

wrld_dorling <- cartogram_dorling(wrld_simpl, "wos", m_weight = 0.3)

colrs <- RColorBrewer::brewer.pal(3, "Set2")


svg(paste("plots/cartograms/cartogram_wos.svg"), width = 8, height = 6)

plot.new()

plot(wrld_dorling, col= "blue", border ="white", bg = "white")

cartography::layoutLayer(title = "WoS", 
                         sources = "", north = TRUE, tabtitle = TRUE,
                         theme = "black.pal", frame = TRUE,  
                         author = "ROAD")

cartography::labelLayer(x = wrld_dorling, txt = "ISO2", overlap = FALSE, show.lines = FALSE, 
                        halo = TRUE, r = .15)

dev.off()

######

wrld_dorling <- cartogram_dorling(wrld_simpl, "openalex", m_weight = 0.3)

colrs <- RColorBrewer::brewer.pal(3, "Set2")


svg(paste("plots/cartograms/cartogram_openalex.svg"), width = 8, height = 6)

plot.new()

plot(wrld_dorling, col= "blue", border ="white", bg = "white")

cartography::layoutLayer(title = "OpenAlex", 
                         sources = "", north = TRUE, tabtitle = TRUE,
                         theme = "black.pal", frame = TRUE,  
                         author = "ROAD")

cartography::labelLayer(x = wrld_dorling, txt = "ISO2", overlap = FALSE, show.lines = FALSE, 
                        halo = TRUE, r = .15)

dev.off()


######

wrld_dorling <- cartogram_dorling(wrld_simpl, "doaj", m_weight = 0.3)

colrs <- RColorBrewer::brewer.pal(3, "Set2")


svg(paste("plots/cartograms/cartogram_doaj.svg"), width = 8, height = 6)

plot.new()

plot(wrld_dorling, col= "blue", border ="white", bg = "white")

cartography::layoutLayer(title = "DOAJ", 
                         sources = "", north = TRUE, tabtitle = TRUE,
                         theme = "black.pal", frame = TRUE,  
                         author = "ROAD")

cartography::labelLayer(x = wrld_dorling, txt = "ISO2", overlap = FALSE, show.lines = FALSE, 
                        halo = TRUE, r = .15)

dev.off()

######

wrld_dorling <- cartogram_dorling(wrld_simpl, "sherpa", m_weight = 0.3)

colrs <- RColorBrewer::brewer.pal(3, "Set2")


svg(paste("plots/cartograms/cartogram_sherpa.svg"), width = 8, height = 6)

plot.new()

plot(wrld_dorling, col= "blue", border ="white", bg = "white")

cartography::layoutLayer(title = "Sherpa", 
                         sources = "", north = TRUE, tabtitle = TRUE,
                         theme = "black.pal", frame = TRUE,  
                         author = "ROAD")

cartography::labelLayer(x = wrld_dorling, txt = "ISO2", overlap = FALSE, show.lines = FALSE, 
                        halo = TRUE, r = .15)

dev.off()

######

wrld_dorling <- cartogram_dorling(wrld_simpl, "topfactor", m_weight = 0.3)

colrs <- RColorBrewer::brewer.pal(3, "Set2")


svg(paste("plots/cartograms/cartogram_topfactor.svg"), width = 8, height = 6)

plot.new()

plot(wrld_dorling, col= "blue", border ="white", bg = "white")

cartography::layoutLayer(title = "Top Factor", 
                         sources = "", north = TRUE, tabtitle = TRUE,
                         theme = "black.pal", frame = TRUE,  
                         author = "ROAD")

cartography::labelLayer(x = wrld_dorling, txt = "ISO2", overlap = FALSE, show.lines = FALSE, 
                        halo = TRUE, r = .15)

dev.off()