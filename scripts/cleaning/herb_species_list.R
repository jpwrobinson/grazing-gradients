
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
setwd('C:/Users/jan/Box Sync/PhD Feedbacks/Git Repository/grazing-gradients')
library(tidyverse); library(funk)
theme_set(theme_sleek()); library(knitr); library(kableExtra)

## load raw fish data
load('data/wio_gbr_herb_master.Rdata')

## extract species
csp<-as.character(unique(herb$species[herb$dataset=='Chagos']))
gsp<-as.character(unique(herb$species[herb$dataset=='GBR']))
ssp<-as.character(unique(herb$species[herb$dataset=='Seychelles']))
msp<-as.character(unique(herb$species[herb$dataset=='Maldives']))

species<-data.frame(species = unique(herb$species))
species$Family<-herb$Family[match(species$species, herb$species)]
species$FG<-herb$FG[match(species$species, herb$species)]

species$chagos<-ifelse(species$species %in% csp, 'TRUE', 'FALSE')
species$gbr<-ifelse(species$species %in% gsp, 'TRUE', 'FALSE')
species$maldives<-ifelse(species$species %in% msp, 'TRUE', 'FALSE')
species$seychelles<-ifelse(species$species %in% ssp, 'TRUE', 'FALSE')

species$nregions<-rowSums(species[,3:6]==T)

dim(species[species$nregions == 4,1])

## 14 browsers, 52 grazers, 38 scrapers
## 104 species in total
## 64 species only appear in 1 dataset

dfbr <- read_csv("data/bite-rates/bite_rate_clean.csv")
speciesbr <- as.character(unique(dfbr$sp))
species$biterates<-ifelse(species$species %in% speciesbr, 'TRUE', 'FALSE')
species$biterates

kable(species, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = 'writing/ms/TableS1_Specieslist.html')

## drop browsers, drop 'herbivore' from FG
species <- species %>% filter(FG != 'Herbivore Browser')
species$FG <- str_replace_all(species$FG, 'Herbivore', '')

## also save csv
write.csv(species, file='writing/ms/TableS1_Specieslist.csv')

