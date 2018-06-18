
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

chag<-list(fish=unique(chagos.fish$site), benthic=unique(chagos$site))

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



maldives.fish<-herb %>% filter(dataset=='Maldives')
## fish sites
unique(maldives.fish$reef)
unique(maldives.fish$site)
# unique(maldives$unique.id)
## benthic sites
unique(maldives$reef)
unique(maldives$site)

mald<-list(fish = unique(maldives.fish$site), benthic = c(as.character(unique(maldives$reef)), 'Huvadhoo'))


write.csv(chag[[1]], file='data/chagos_fish_sites.csv')
write.csv(chag[[2]], file='data/chagos_benthic_sites.csv')
write.csv(mald[[1]], file='data/maldives_fish_sites.csv')
write.csv(mald[[2]], file='data/maldives_benthic_sites.csv')