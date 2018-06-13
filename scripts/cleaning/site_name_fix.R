
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())

## load raw data
load('data/wio_gbr_herb_master.Rdata')
load(file='data/wio_herb_benthic.Rdata')
load(file='data/wio_benthic_master.Rdata')
## need to fix chagos and GBR for matching

## chagos
chagos.fish<-herb %>% filter(dataset=='Chagos')
## fish sites
unique(chagos.fish$reef)
unique(chagos.fish$site)
# unique(chagos$unique.id)
## benthic sites
unique(chagos$reef)
unique(chagos$site)

## Chagos missing reefs + sites

## gbr
gbr.fish<-herb %>% filter(dataset=='GBR')
## fish sites
unique(gbr.fish$reef)
unique(gbr.fish$site)
# unique(gbr$unique.id)
## benthic sites
unique(gbr$reef)
unique(gbr$site)

### GBR is fixable.