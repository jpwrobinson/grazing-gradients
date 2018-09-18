
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())
uniques<-function(x){length(unique(x))}

load('data/wio_gbr_herb_master.Rdata')

## load uncleaned complexity data
maldives<-read.csv('data/raw-data/csv/maldives_complexity.csv')
gbr<-read.csv('data/raw-data/csv/gbr_complexity.csv')
chagos<-read.csv('data/raw-data/csv/chagos_complexity.csv')


## fix sites which have more info in the unique id
## this all follows from clean_merge_benthic_data.R naming scheme
chagos$Island<-as.character(chagos$Island)
chagos$Island[chagos$Unique_site_transect == 'Great Chagos BankSouth BrotherExposed1']<-'South Brother (E1)'
chagos$Island[chagos$Unique_site_transect == 'Great Chagos BankSouth BrotherExposed2']<-'South Brother (E2)'
chagos$Island[chagos$Unique_site_transect == 'Great Chagos BankEagleSheltered1']<-'Eagle (S1)'
chagos$Island[chagos$Unique_site_transect == 'Great Chagos BankEagleSheltered2']<-'Eagle (S2)'
chagos$Island[chagos$Unique_site_transect == 'Great Chagos BankEagleSheltered3']<-'Eagle (S3)'
chagos$Island[chagos$Unique_site_transect == 'SalamonIle AnglaisSheltered1']<-'Isle le Anglaise (S1)'
chagos$Island[chagos$Unique_site_transect == 'SalamonIle AnglaisSheltered2']<-'S2'
chagos$Island[chagos$Unique_site_transect == 'SalamonIle AnglaisSheltered3']<-'Isle le Anglaise (S3)'


## Maldives
head(pred %>% filter(dataset == 'Maldives'))
maldives$Island<-str_replace_all(maldives$Island, 'Vodamuloo', 'Vodamulaa')

maldives$unique.id<-with(maldives, paste(Island, 'Huvadhoo', sep='.'))
colnames(maldives)<-c('dataset', 'reef', 'habitat', 'site', 'depth', 'transect', 'complexity', 'unique.id')



## average across replicates
maldives<-maldives %>% group_by(dataset, reef, site, unique.id) %>% summarise(complexity = mean(complexity))
head(maldives)

## GBR
head(pred %>% filter(dataset == 'GBR'))
gbr$site <-with(gbr, paste0(Reef, Site))
gbr$unique.id<-with(gbr, paste(site, Reef, sep='.'))
colnames(gbr)<-c('dataset', 'reef', 'habitat', 'site.number', 'zone', 'replicate', 'complexity', 'site', 'unique.id')

## average across replicates
gbr<-gbr %>% group_by(dataset, reef, site, unique.id) %>% summarise(complexity = mean(complexity))


## Chagos
head(pred %>% filter(dataset == 'Chagos'))
chagos$unique.id<-with(chagos, paste(Atoll, Island, sep='.'))
colnames(chagos)<-c('dataset', 'reef', 'site', 'habitat', 'unique.transect', 'depth', 'transect', 'complexity', 'unique.id')

## average across replicates
chagos<-chagos %>% group_by(dataset, reef, site, unique.id) %>% summarise(complexity = mean(complexity))
head(chagos)


## change factors to characters
maldives$dataset<-as.character(maldives$dataset)
chagos$dataset<-as.character(chagos$dataset)
gbr$dataset<-as.character(gbr$dataset)

maldives$site<-as.character(maldives$site)
chagos$site<-as.character(chagos$site)
gbr$site<-as.character(gbr$site)

maldives$reef<-as.character(maldives$reef)
chagos$reef<-as.character(chagos$reef)
gbr$reef<-as.character(gbr$reef)

## fix site names to match with benthic
chagos$site[chagos$site=='Exposed site 1']<-'Petite Coq (E4)'
chagos$site[chagos$site=='Exposed site 2']<-'South Coq (E5)'
chagos$site[chagos$site=='Exposed site 3']<-'South Coq (E6)'
chagos$site[chagos$site=='Ile Fouquet']<-'E3'
chagos$site[chagos$site=='Ile Poule']<-'Ile Poule (S1)'
chagos$site[chagos$site=='Ile Takamaka']<-'E2'
chagos$site[chagos$site=='Middle Brother']<-'Middle Brother (E3)'
chagos$site[chagos$site=='Sheltered site 2']<-'Ile Poule (S2)'
chagos$site[chagos$site=='Sheltered site 3']<-'S3'
chagos$site[chagos$site=='South']<-'E4'
chagos$unique.id<-with(chagos, paste(reef, site, sep='.'))

complex<-rbind(chagos, maldives, gbr)
save(complex, file = 'data/wio_complexity.Rdata')