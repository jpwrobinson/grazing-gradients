
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())

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

## 14 browsers, 52 grazers, 39 scrapers
## 105 species in total
## 65 species only appear in 1 dataset


write.csv(species, 'data/herbivore_species_list.csv')