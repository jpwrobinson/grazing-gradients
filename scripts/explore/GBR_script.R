setwd("~/Box Sync/PhD Feedbacks/Git Repository/grazing-gradients")

load('data/wio_gbr_herb_master.Rdata')
str(herb)
unique(herb$dataset)
gbr <- herb[herb$dataset == 'GBR',]
write.csv(gbr, file = 'data/gbr_master.csv')

library(ggplot2)
library(tidyverse)

load('data/gbr_master.csv')
gbr <- gbr_master

str(gbr)
unique(gbr$date) #Nov 2010 & Jan 2011
unique(gbr$site) #5 reefs, 3 sites each, 4 transects each
unique(gbr$management) #only 1 level
unique(gbr$habitat) #3 exposed, 3 sheltered, crest-flat-slope for each
unique(gbr$depth) #NA
unique(gbr$family) #8 families
unique(gbr$FG) #3 funtional groups
unique(gbr$species) #71 species
aggregate(species ~ FG, gbr, function(x) length(unique(x))) #10 browsers, 40 grazers, 21 scrapers

#biomass by functional group
ggplot(gbr, aes(log(mass.g, 2), fill=FG)) + geom_histogram(breaks=c(1:10)) + 
  facet_wrap(~FG) + theme(legend.position='none') + scale_x_continuous(breaks=c(1:10), 
  labels=c(2^c(1:10))) + labs (x='Mass (g)')

#common species
abund.gbr <- gbr %>% group_by(unique.id, site, species, FG) %>% 
  summarise(abund.gbr = sum(abundance.500m2)) %>%
  group_by(species, FG) %>% 
  summarise(abund.gbr = mean(abund.gbr))

biom.gbr <- gbr %>% group_by(unique.id, site, species, FG) %>% 
  summarise(biom.gbr = sum(biomass.kgha)) %>%
  group_by(species, FG) %>% 
  summarise(biom.gbr = mean(biom.gbr))

ggplot(abund.gbr[abund.gbr$abund.gbr<100,], aes(reorder(species,abund.gbr), 
  abund.gbr, fill=FG)) + geom_bar(stat='identity') + 
  labs(y='Mean abundance per 500m2', x='',title='GBR: abundance/biomass distributions') +
  coord_flip() + theme(legend.position='none', axis.text.y=element_text(size=6))

ggplot(biom.gbr[biom.gbr$biom.gbr<300,], aes(reorder(species,biom.gbr),
  biom.gbr, fill=FG)) + geom_bar(stat='identity') + 
  labs(y='Mean biomass kg ha-1', x='') +
  coord_flip() + theme(legend.position=c(0.75, 0.25), axis.text.y=element_text(size=6))

#ugly biomass gradient across habitat for later
ggplot(gbr, aes(log(mass.g, 2), fill=FG)) + geom_histogram(breaks=c(1:10)) + 
  facet_wrap(habitat~FG) + theme(legend.position='none') + 
  scale_x_continuous(breaks=c(1:10), labels=c(2^c(1:10))) + labs (x='Mass (g)')
