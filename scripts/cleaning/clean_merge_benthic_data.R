
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())

## load raw fish data
load('data/wio_gbr_herb_master.Rdata')

## load uncleaned benthic data
maldives<-read.csv('data/raw-data/csv/maldives_benthos.csv')
gbr<-read.csv('data/raw-data/csv/gbr_benthos.csv')
chagos<-read.csv('data/raw-data/csv/chagos_benthos.csv')

## load seychelles (clean)
load('data/raw-data/SEY_UVC_benthicPV_SC_DEPTH_replicates.Rdata')
seychelles<-SC.site; rm(SC.site)
## change column names to match fish data
colnames(seychelles)[colnames(seychelles)=='Island']<-'reef'
colnames(seychelles)[colnames(seychelles)=='location']<-'site'
colnames(seychelles)[colnames(seychelles)=='count']<-'transect'
seychelles$dataset<-'Seychelles'

## gather taxa into 1 column
seychelles<-gather(seychelles,  taxa, value, -site, -habitat, -transect, -year, -depth, -site.year, -reef, -state)

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
algae<-c('CCA', 'EAM', 'Macroalgae')

maldives$benthic<-ifelse(maldives$taxa %in% other, 'other', 'hardcoral')
maldives$benthic<-ifelse(maldives$taxa %in% availsubtrate, 'availablesubtrate', maldives$benthic)
maldives$benthic<-ifelse(maldives$taxa %in% algae, 'algae', maldives$benthic)

## now aggregate by benthic category
maldives <- maldives %>% group_by(dataset, site, date, reef, transect, depth, benthic) %>%
			summarise(value = sum(value))

## double check covers sum to 100
maldives %>% group_by(dataset, site, date, reef, transect, depth) %>%
			summarise(value = sum(value)) %>% filter(value != 100)

## need to check Maarehaa transect 6 = 2 depths.


## ------------------------------------ ##
		   ## clean chagos ##
## ------------------------------------ ##

## ------------------------------------ ##
		   ## clean GBR ##
## ------------------------------------ ##
