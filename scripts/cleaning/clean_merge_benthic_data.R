
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())

## load raw fish data
load('data/wio_gbr_herb_master.Rdata')

## load uncleaned benthic data
maldives<-read.csv('data/raw-data/csv/maldives_benthos.csv')
gbr<-read.csv('data/raw-data/csv/gbr_benthos.csv')
chagos<-read.csv('data/raw-data/csv/chagos_benthos.csv', header=F)


## load seychelles (clean)
load('data/raw-data/SEY_UVC_benthicPV_SC_DEPTH_replicates.Rdata')
seychelles<-SC.site; rm(SC.site)
## change column names to match fish data
colnames(seychelles)[colnames(seychelles)=='Island']<-'reef'
colnames(seychelles)[colnames(seychelles)=='location']<-'site'
colnames(seychelles)[colnames(seychelles)=='count']<-'transect'
seychelles$dataset<-'Seychelles'

## gather taxa into 1 column
seychelles<-gather(seychelles,  benthic, value, -site, -habitat, -transect, -year, -depth, -site.year, -reef, -state)

## drop extra taxa for now to focus on total hard.coral and macroalgae
seychelles<-seychelles %>% filter(benthic %in% c('hard.coral', 'macroalgae'))

## change to numeric
seychelles$value<-as.numeric(seychelles$value)

# add unique ID
seychelles$unique.id<-with(seychelles, paste(year, site, sep='.'))

## average across replicates to get site level values
seychelles <- seychelles %>% group_by(site, habitat, year, site.year, reef, state, benthic, unique.id) %>%
				summarise(cover = mean(value))

## ------------------------------------ ##
		   ## clean maldives ##
## ------------------------------------ ##

colnames(maldives)<-c('dataset', 'site', 'date', 'reef', 'transect', 'depth', 'distance', 'value', 'taxa')
## estimates are line transect at fixed intervals (0.5m for 50m transect). 
## sum intervals per taxa and /100 for percent cover
maldives <- maldives %>% group_by(dataset, site, date, reef, transect, depth, taxa) %>%
			summarise(value = sum(value)/100)

## need to group taxa for simpler comparison across datasets + benthic gradients
availsubtrate<-c('Rock', 'Sand', 'Rubble')
other<-c('Fungia', 'Soft Coral Encrusting', 'Zoanthid', 'Hydroid', 'Ascidian', 
	'Sponge Encrusting', 'Anemone', 'Coralimorph', 'Tubastrea', 
	'Heliopora', 'Soft Coral Table')
turf<-c('CCA', 'EAM')
macroalgae<-'Macroalgae'

maldives$benthic<-ifelse(maldives$taxa %in% other, 'other', 'hard.coral')
maldives$benthic<-ifelse(maldives$taxa %in% availsubtrate, 'availablesubtrate', maldives$benthic)
maldives$benthic<-ifelse(maldives$taxa %in% turf, 'turf', maldives$benthic)
maldives$benthic<-ifelse(maldives$taxa %in% macroalgae, 'macroalgae', maldives$benthic)

## now aggregate by benthic category
maldives <- maldives %>% group_by(dataset, site, date, reef, transect, depth, benthic) %>%
			summarise(value = sum(value))

## double check covers sum to 100
maldives %>% group_by(dataset, site, date, reef, transect, depth) %>%
			summarise(value = sum(value)) %>% filter(value != 100)

## need to check Maarehaa transect 6 = 2 depths.

## keep only total coral and macroalgae
maldives <- maldives %>% filter(benthic %in% c('hard.coral', 'macroalgae'))


## add ID
maldives$unique.id<-with(maldives, paste(reef, site, sep='.'))


## now average across replicates
maldives <- maldives %>% group_by(dataset, site, date, reef, benthic, unique.id) %>%
				summarise(cover = mean(value))


## ------------------------------------ ##
		   ## clean chagos ##
## ------------------------------------ ##

rownames(chagos)<-chagos$V1
chagos$V1<-NULL

## big ol' transpose
chagos<-chagos %>%
   rownames_to_column %>% 
   gather(var, value, -rowname) %>% 
   spread(rowname, value) 


## drop some nonsense
chagos$var<-NULL; chagos$'Total no. points'<-NULL

## now gather taxa and keep ID vars
chagos <- chagos %>% gather(taxa, value, -Date, -'Depth/Zone', -Reef, -Replicate, -Site)
## add dataset
chagos$dataset<-'Chagos'
##fix colnames
colnames(chagos)<-c('date', 'depth', 'reef', 'transect', 'site', 'taxa', 'value', 'dataset')


## identify benthic vars of interest
chagos$benthic<-ifelse(chagos$taxa == 'Total.hard.coral.cover', 'hard.coral', NA)
chagos$benthic<-ifelse(chagos$taxa == 'Halimeda', 'macroalgae', chagos$benthic)

## sum benthic categories 
chagos$value<-as.numeric(chagos$value)
chagos <- chagos %>% group_by(dataset, site, date, reef, transect, depth, benthic) %>%
			summarise(value = sum(value))
chagos$value[is.na(chagos$value)]<-0

## keep only total coral and macroalgae
chagos <- chagos %>% filter(benthic %in% c('hard.coral', 'macroalgae'))

head(chagos)

## add ID
chagos$unique.id<-with(chagos, paste(reef, site, transect, sep='.'))

## average across replicates
chagos <- chagos %>% group_by(dataset, site, date, reef, benthic, unique.id) %>%
				summarise(cover = mean(value))


## ------------------------------------ ##
		   ## clean GBR ##
## ------------------------------------ ##
## drop 3 NAs
gbr <- gbr[!gbr$Reef=='',]
colnames(gbr)[1:6]<-c('date', 'site', 'site.number', 'habitat', 'depth', 'transect')

## gather taxa into 1 column
gbr <- gather(gbr, taxa, value, -date, -site, -site.number, -habitat, -depth, -transect)
gbr$dataset<-'GBR'
gbr$reef<-paste0(gbr$site, gbr$site.number)


## estimates are already percent cover
## need to group taxa for simpler comparison across datasets + benthic gradients
availsubtrate<-c('Pavement', 'Sand', 'Rubble', 'Bare.Substrate')
# other<-c('Fungia', 'Soft Coral Encrusting', 'Zoanthid', 'Hydroid', 'Ascidian', 
# 	'Sponge Encrusting', 'Anemone', 'Coralimorph', 'Tubastrea', 
# 	'Heliopora', 'Soft Coral Table')
turf<-c('Turf', 'Turfs', 'CCA.1', 'Filimentous.Green','Ulva', 'CCA', 'EAM') 
macroalgae<-c('Unidentified.Macroalgae', 'Halimeda','Macroalgae')

gbr$benthic[gbr$taxa %in% availsubtrate]<- 'availablesubtrate'
gbr$benthic[gbr$taxa %in% macroalgae]<- 'macroalgae'
gbr$benthic[gbr$taxa %in% turf]<- 'turf'
gbr$benthic[gbr$taxa == 'Total.hard.coral.cover']<- 'hard.coral'

## drop missing categories (i.e. non coral, algal or substrate groups)
gbr<-gbr[-which(is.na(gbr$benthic)),]

## sum groups within each benthic category
gbr <- gbr %>% group_by(dataset, site, date, reef, transect, depth, benthic) %>%
			summarise(value = sum(value))

gbr$value[is.na(gbr$value)]<-0

## keep only total coral and macroalgae
gbr <- gbr %>% filter(benthic %in% c('hard.coral', 'macroalgae'))

## add ID
gbr$unique.id<-with(gbr, paste(reef, site, sep='.'))


gbr <- gbr %>% group_by(dataset, site, date, reef, benthic, unique.id) %>%
				summarise(cover = mean(value))


## chagos site names are fucked
## maldives site names are fucked
## save individual
save(chagos, gbr, maldives, seychelles, file='data/wio_benthic_master.Rdata')


pred<-herb

## add IDs separately for each dataset
pred$unique.id[pred$dataset == 'Seychelles'] <- with(pred[pred$dataset == 'Seychelles',], paste(date,site, sep='.'))
# pred$unique.id[pred$dataset == 'Chagos'] <- with(pred[pred$dataset == 'Chagos',], paste(site, reef, sep='.'))
# pred$unique.id[pred$dataset == 'Maldives'] <- with(pred[pred$dataset == 'Maldives',], paste(site, reef, sep='.'))
pred$unique.id[pred$dataset == 'GBR'] <- with(pred[pred$dataset == 'GBR',], paste(site, reef, sep='.'))


# pred$hard.coral[pred$dataset=='Maldives']<-maldives$cover[maldives$benthic=='hard.coral'][match(pred$unique.id, maldives$unique.id)]
# pred$hard.coral[pred$dataset=='Chagos']<-chagos$cover[chagos$benthic=='hard.coral'][match(pred$unique.id, chagos$unique.id)]
pred$hard.coral<-seychelles$cover[seychelles$benthic=='hard.coral'][match(pred$unique.id, seychelles$unique.id)]
pred$hard.coral<-gbr$cover[gbr$benthic=='hard.coral'][match(pred$unique.id, gbr$unique.id)]


## save master
save(pred, file='data/wio_herb_benthic.Rdata')



