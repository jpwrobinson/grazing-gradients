
setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(dplyr); library(tidyr); library(funk)
theme_set(theme_sleek())
load(file='data/wio_gbr_fish_master.Rdata'); fish<-herb

### applying same cleaning process as in 'clean_merge_benthic_data.R' to get site names fixed
## drop seychelles 1994, 2005
fish<-fish %>% filter(!(date %in% c(1994, 2005)))

### fix chagos site names to match with benthos
sites<-read.csv('data/raw-data/csv/chagos_sitenames.csv')
fish$site[fish$site=='Barton Point']<-'Barton Point east'
fish$site[fish$site=='Diego Garcia East coast']<-'East side'
fish$site[fish$site=='Exposed site 1']<-'Petite Coq (E4)'
fish$site[fish$site=='Exposed site 2']<-'South Coq (E5)'
fish$site[fish$site=='Exposed site 3']<-'South Coq (E6)'
fish$site[fish$site=='Ile Fouquet']<-'E3'
fish$site[fish$site=='Ile Poule']<-'Ile Poule (S1)'
fish$site[fish$site=='Ile Takamaka']<-'E2'
fish$site[fish$site=='Middle Brother']<-'Middle Brother (E3)'
fish$site[fish$site=='Sheltered site 2']<-'Ile Poule (S2)'
fish$site[fish$site=='Sheltered site 3']<-'S3'
fish$site[fish$site=='South']<-'E4'

## fix sites which have more info in the unique id
fish$site[fish$unique.id == 'Great Chagos BankSouth BrotherExposed1']<-'South Brother (E1)'
fish$site[fish$unique.id == 'Great Chagos BankSouth BrotherExposed2']<-'South Brother (E2)'

fish$site[fish$unique.id == 'Great Chagos BankEagleSheltered1']<-'Eagle (S1)'
fish$site[fish$unique.id == 'Great Chagos BankEagleSheltered2']<-'Eagle (S2)'
fish$site[fish$unique.id == 'Great Chagos BankEagleSheltered3']<-'Eagle (S3)'

## S3 is labelled for Peuros Banhos and Salamon. Introduce new ID
fish$site[fish$unique.id == 'SalamonIle AnglaisSheltered1']<-'Isle le Anglaise (S1)'
fish$site[fish$unique.id == 'SalamonIle AnglaisSheltered2']<-'S2'
fish$site[fish$unique.id == 'SalamonIle AnglaisSheltered3']<-'Isle le Anglaise (S3)'



## add IDs separately for each dataset
fish$unique.id<-as.character(fish$unique.id)
fish$unique.id[fish$dataset == 'Seychelles'] <- with(fish[fish$dataset == 'Seychelles',], paste(date,site, sep='.'))
fish$unique.id[fish$dataset == 'Chagos'] <- with(fish[fish$dataset == 'Chagos',], paste(reef, site, sep='.'))
fish$unique.id[fish$dataset == 'Maldives'] <- with(fish[fish$dataset == 'Maldives',], paste(reef, site, sep='.'))
fish$unique.id[fish$dataset == 'GBR'] <- with(fish[fish$dataset == 'GBR',], paste(site, reef, sep='.'))



### estimate total fishable biomass at each site
### defined as the total biomass of all observed fish on ONE TRANSECT, averaged ACROSS TRANSECTS
biom <- fish %>% group_by(unique.id, transect) %>%
					summarise(biom = sum(biomass.kgha)) %>%
					ungroup() %>%
					group_by(unique.id) %>%
					summarise(biom = mean(biom))

save(biom, file = 'data/wio_gbr_fishable_biom.Rdata')

