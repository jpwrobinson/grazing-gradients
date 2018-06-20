rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek()); library(stringr)



chagos<-read.csv('data/raw-data/csv/chagos_fish.csv')
gbr<-read.csv('data/raw-data/csv/gbr_fish.csv')
maldives<-read.csv('data/raw-data/csv/maldives_fish.csv')
load('data/raw-data/SEY_UVC_fish_clean_1994-2017.Rdata'); seychelles<-fish

## drop columns and change colnames
colnames(gbr)[colnames(gbr)=='Functional.Group']<-'FG'
colnames(seychelles)[colnames(seychelles)=='FG.fine']<-'FG'

## change to character to help merging over levels, and fix to common system
chagos$FG<-as.character(chagos$FG); unique(chagos$FG)
chagos$FG<-plyr::revalue(chagos$FG, replace=c('Detritivore & Herbivore Grazer' = 'Herbivore Grazer',
										'Herbivore Excavator' = 'Herbivore Scraper',
										'Herbivore Scraper/Small Excavator' = 'Herbivore Scraper',
										'Herbivore Scarper/Small Excavator' = 'Herbivore Scraper'))

gbr$FG<-as.character(gbr$FG); unique(gbr$FG)
gbr$FG<-plyr::revalue(gbr$FG, replace=c('Detritivore & Herbivore Grazer' = 'Herbivore Grazer',
										'Herbivore Excavator' = 'Herbivore Scraper',
										'Herbivore Scraper/Small Excavator' = 'Herbivore Scraper'))

seychelles$FG<-as.character(seychelles$FG); unique(seychelles$FG)
seychelles$FG<-plyr::revalue(seychelles$FG, replace=c('Herbivore Excavator' = 'Herbivore Scraper'))

## add maldives FG info
maldives$FG<-gbr$FG[match(maldives$Species, gbr$Species)]
maldives$FG[is.na(maldives$FG)]<-chagos$FG[match(maldives$Species[is.na(maldives$FG)], chagos$Species)]
maldives$FG[is.na(maldives$FG)]<-seychelles$FG[match(maldives$Species[is.na(maldives$FG)], seychelles$Species)]

unique(maldives$Species[is.na(maldives$FG)]) ## 27 species no FG info, some herbs in here
## assign manually but make note to check with Nick
maldives$FG[maldives$Species=='Chlorurus enneacanthus']<-'Herbivore Scraper'
## unknown: Pomacentrus philippinus
## remaining are non-herbivores

## keep only herb species
chagos<-chagos[grepl('Herb*', chagos$FG),]
unique(chagos$FG)

gbr<-gbr[grepl('Herb*', gbr$FG),]
unique(gbr$FG)

seychelles<-seychelles[grepl('Herb*', seychelles$FG),]
unique(seychelles$FG)

maldives<-maldives[grepl('Herb*', maldives$FG),]
unique(maldives$FG)



## CHAGOS CLEANING FOR MERGE ##
## add date column

chagos$date<-NA
# chagos$Unique_Site_ID<-paste(gsub('\\ ', '',chagos$Location.Atoll), gsub('\\ ', '', chagos$Island.Site), chagos$Transect, sep='.')
chagos$Unique_Site_ID<-chagos$Unique_site_transect
chagos$mass.g<-NA
chagos$Depth<-as.numeric(str_replace_all(chagos$Depth, 'm', ''))
chagos$management<-'Unfished'
chagos$dataset<-'Chagos'
## now subset to relevant columns and assign identical colnames
chagos<-chagos %>% select(date, dataset, Location.Atoll, Island.Site, Site, management, Exposure, Unique_Site_ID, Depth, Transect, Transect.area, Family, Species, FG, Length, Biomass..g., Biomass..kg.ha., Abundance.500m2)
colnames(chagos)<-c('date','dataset', 'reef', 'site', 'site.number','management','habitat', 'unique.id', 'depth', 'transect', 'transect.area', 'family', 'species', 'FG', 'length.cm','mass.g', 'biomass.kgha', 'abundance.500m2')

## fix species names
firstup <- function(x) {
   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
x
}

chagos$species<-as.character(chagos$species)
chagos$species<-firstup(chagos$species)
## fix scarus falcipinnis
chagos$species[chagos$species=='Scarus falcippinis']<-'Scarus falcipinnis'
chagos$species<-as.factor(chagos$species)


## GBR CLEANING FOR MERGE ##
## create new habitat, site and depth vars
gbr$habitat<-with(gbr, paste(Exposure, Zone, sep='.'))
gbr$site<-with(gbr, paste(Reef, Site, sep=''))
gbr$depth<-NA
gbr$Unique_Site_ID<-with(gbr, paste(reef, site, Transect, sep='.'))
gbr$management<-'Unfished'
gbr$dataset<-'GBR'
## now subset to relevant columns and assign identical colnames
gbr<-gbr %>% select(Date,dataset, Reef, site, Site, management, habitat, Unique_Site_ID, depth, Transect, Transect.size, Family, Species, FG, Length, Weight,Biomass..kg.ha., Abundance...500m2.)
colnames(gbr)<-c('date','dataset', 'reef', 'site', 'site.number','management','habitat', 'unique.id', 'depth', 'transect', 'transect.area', 'family', 'species', 'FG', 'length.cm','mass.g', 'biomass.kgha', 'abundance.500m2')

## Maldives CLEANING FOR MERGE ##
## create new habitat, site and depth vars
maldives$site<-with(maldives, paste(Location.Atoll, Island.Site, sep='.'))
maldives$Depth<-as.numeric(str_replace_all(maldives$Depth, 'm', ''))
maldives$Unique_site_transect<-with(maldives, paste(Island.Site, Exposure, Transect, sep='.'))
maldives$management<-'Fished'
maldives$dataset<-'Maldives'
## now subset to relevant columns and assign identical colnames
maldives<-maldives %>% select(Date,dataset, Location.Atoll, Island.Site, site, management, Exposure, Unique_site_transect, Depth, Transect, Transect.area, Family, Species, FG, Length,Biomass..g., Biomass..kg.ha., Abundance.500m2)
colnames(maldives)<-c('date','dataset', 'reef', 'site', 'site.number','management','habitat', 'unique.id', 'depth', 'transect', 'transect.area', 'family', 'species', 'FG', 'length.cm', 'mass.g','biomass.kgha', 'abundance.500m2')

maldives$species<-as.character(maldives$species)
maldives$species<-firstup(maldives$species)
maldives$species<-as.factor(maldives$species)



## Seychelles CLEANING FOR MERGE ##
## create new habitat, site and depth vars
seychelles$site.number<-as.numeric(factor(seychelles$Location))
seychelles$Island<-ifelse(grepl('Mahe*', seychelles$Location), 'Mahe', 'Praslin')
seychelles$Island<-ifelse(grepl('Ste Anne*', seychelles$Location), 'Mahe', seychelles$Island)
seychelles$Depth<-NA
seychelles$Unique_site_transect<-with(seychelles, paste(Year, Location, count, sep='.'))
seychelles$Transect.area<-NA
seychelles$dataset<-'Seychelles'
## now subset to relevant columns and assign identical colnames
seychelles<-seychelles %>% select(Year, dataset, Island, Location, site.number, Management, Habitat, Unique_site_transect, Depth, count, Transect.area, Family, Species, FG, length, mass.g, biomass.kgha, abundance.500m2)
colnames(seychelles)<-c('date','dataset', 'reef', 'site', 'site.number','management', 'habitat', 'unique.id', 'depth', 'transect', 'transect.area', 'family', 'species', 'FG', 'length.cm','mass.g', 'biomass.kgha', 'abundance.500m2')

## fix problematic factors before merge
chagos$habitat<-as.character(chagos$habitat)
gbr$habitat<-as.character(gbr$habitat)
maldives$habitat<-as.character(maldives$habitat)
seychelles$habitat<-as.character(seychelles$habitat)

chagos$reef<-as.character(chagos$reef)
gbr$reef<-as.character(gbr$reef)
maldives$reef<-as.character(maldives$reef)
seychelles$reef<-as.character(seychelles$reef)

chagos$site<-as.character(chagos$site)
gbr$site<-as.character(gbr$site)
maldives$site<-as.character(maldives$site)
seychelles$site<-as.character(seychelles$site)

chagos$species<-as.character(chagos$species)
gbr$species<-as.character(gbr$species)
maldives$species<-as.character(maldives$species)
seychelles$species<-as.character(seychelles$species)

### merge and drop non-herbivores
herb<-rbind(chagos, gbr, seychelles, maldives)
herb<-herb[grepl('Herb*', herb$FG),]

save(herb, file='data/wio_gbr_herb_master.Rdata')


