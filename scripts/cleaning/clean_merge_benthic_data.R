
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())
uniques<-function(x){length(unique(x))}

## load raw fish data
load('data/wio_gbr_herb_master.Rdata')


## load uncleaned benthic data
maldives<-read.csv('data/raw-data/csv/maldives_benthos.csv')
seychelles<-read.csv('data/raw-data/csv/seychelles_benthos.csv')
gbr<-read.csv('data/raw-data/csv/gbr_benthos.csv')
chagos<-read.csv('data/raw-data/csv/chagos_benthos.csv', header=F)
chagos2<-read.csv('data/raw-data/csv/chagos_benthos2.csv', header=T)


## change column names to match fish data
colnames(seychelles)[colnames(seychelles)=='location']<-'site'
colnames(seychelles)[colnames(seychelles)=='count']<-'transect'
colnames(seychelles)[colnames(seychelles)=='year']<-'date'
seychelles$dataset<-'Seychelles'
seychelles$unique.id<-with(seychelles, paste(site, date, sep='.'))

## gather taxa into 1 column
seychelles<-gather(seychelles,  taxa, value, -dataset, -site, -habitat, -transect, -date, -unique.id)
## drop Line.inter from characters
seychelles$taxa<-str_replace_all(seychelles$taxa, 'Line.inter.', '')
## fix habitats
seychelles$habitat<-plyr::revalue(seychelles$habitat, c('Granite' = 'Granitic', 'coral'='Carbonate', 'granite'= 'Granitic', 'sand'='Patch'))
## fix sites
seychelles$site<-str_replace_all(seychelles$site, '_Coral', ' Carbonate')
seychelles$site<-str_replace_all(seychelles$site, '_Granite', ' Granite')
seychelles$site<-str_replace_all(seychelles$site, '_Sand', ' Patch')
seychelles$site<-str_replace_all(seychelles$site, '_sand', ' Patch')
seychelles$site<-str_replace_all(seychelles$site, 'Ste.', 'Ste')
seychelles$site<-str_replace_all(seychelles$site, 'SteAnne', 'Ste Anne')


## need to group taxa for simpler comparison across datasets + benthic gradients
benthic.cats<-unique(seychelles$taxa)

# turf<-c('Coraline.algae')
macroalgae<-c('Sargassum', 'Asparagopsis', 'Caulerpa', 'Galaxora', 
		'Lobophyta', 'turtle.grass', 'Halimeda', 'Turbinaria_macroalgae', 
		'Dictyota' , 'Dictyopteris', 'Padina',  'seagrass')
hard.coral<-benthic.cats[c(22:53, 57:65)]; hard.coral

seychelles$benthic<-ifelse(seychelles$taxa %in% hard.coral,  'hard.coral', NA)
# seychelles$benthic<-ifelse(seychelles$taxa %in% turf, 'turf', seychelles$benthic)
seychelles$benthic<-ifelse(seychelles$taxa %in% macroalgae, 'macroalgae', seychelles$benthic)

## save complexity for later
seychelles.complex<-seychelles %>% filter(taxa == 'struc.complexity')
seychelles.complex$unique.id<-with(seychelles.complex, paste(date, site, sep='.'))

## subset to hard coral + MA
seychelles<-seychelles %>% filter(benthic %in% c('hard.coral', 'macroalgae'))

## change to numeric
seychelles$value<-as.numeric(seychelles$value)

# add unique ID
seychelles$unique.id<-with(seychelles, paste(date, site, sep='.'))

## now aggregate by benthic category
seychelles <- seychelles %>% group_by(dataset, site, date,unique.id, habitat, transect, benthic) %>%
			summarise(value = sum(value))
seychelles$value[is.na(seychelles$value)]<-0
## multiply by 10 to get percent cover
seychelles$value<-seychelles$value * 10 



## average across replicates to get site level values
seychelles <- seychelles %>% group_by(site, habitat, date, benthic, unique.id) %>%
				summarise(cover = mean(value))

# ggplot(seychelles, aes(date, cover, col=benthic)) + geom_line() + facet_wrap(~site)


## ------------------------------------ ##
		   ## clean maldives ##
## ------------------------------------ ##

colnames(maldives)<-c('dataset', 'site', 'date', 'reef', 'transect', 'depth', 'distance', 'value', 'taxa')
## estimates are line transect at fixed intervals (0.5m for 50m transect). 
## sum intervals per taxa and /100 for percent cover
maldives <- maldives %>% group_by(dataset, site, date, reef, transect, depth, taxa) %>%
			summarise(value = sum(value)/100)

## fix error site name
maldives$reef<-str_replace_all(maldives$reef, 'Vilingi', 'Vilingili')


## need to group taxa for simpler comparison across datasets + benthic gradients
availsubtrate<-c('Rock', 'Sand', 'Rubble')
other<-c('Fungia', 'Soft Coral Encrusting', 'Zoanthid', 'Hydroid', 'Ascidian', 
	'Sponge Encrusting', 'Anemone', 'Coralimorph', 'Tubastrea', 
	'Heliopora', 'Soft Coral Table')
non.ma<-c('CCA', 'EAM')
macroalgae<-'Macroalgae'

maldives$benthic<-ifelse(maldives$taxa %in% other, 'other', 'hard.coral')
maldives$benthic<-ifelse(maldives$taxa %in% availsubtrate, 'availablesubtrate', maldives$benthic)
maldives$benthic<-ifelse(maldives$taxa %in% non.ma, 'non.ma', maldives$benthic)
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

## fix error name
chagos$reef<-str_replace_all(chagos$reef, 'Peuros', 'Peros')

## fix transect to numbers
chagos$depth<-str_replace_all(chagos$depth, 'm', '')
chagos$depth<-as.integer(chagos$depth)

## identify benthic vars of interest
chagos$benthic<-ifelse(chagos$taxa == 'Total hard coral cover', 'hard.coral', NA)
chagos$benthic<-ifelse(chagos$taxa %in% c('Halimeda'), 'macroalgae', chagos$benthic)



## sum benthic categories 
chagos$value<-as.numeric(chagos$value)
chagos <- chagos %>% group_by(dataset, site, date, reef, transect, depth, benthic) %>%
			summarise(value = sum(value))
chagos$value[is.na(chagos$value)]<-0

## keep only total coral and macroalgae
chagos <- chagos %>% filter(benthic %in% c('hard.coral', 'macroalgae'))

### repeat for chagos 2 - Morgan Pratchett data

## big ol' transpose
chagos2<-chagos2 %>%
   gather(taxa, value, -Location, -Atoll, -Site, -Unique_site_transect, -Depth.Zone, -Replicate) 

chagos2$value[is.na(chagos2$value)]<-0

## add dataset
chagos2$dataset<-'Chagos'; chagos2$Location<-NULL; chagos2$Unique_site_transect<-NULL
##fix colnames
colnames(chagos2)<-c('reef', 'site','depth', 'transect', 'taxa', 'value', 'dataset')


## identify benthic vars of interest
chagos2$benthic<-ifelse(chagos2$taxa == 'Total.hard.coral.cover', 'hard.coral', NA)
chagos2$benthic<-ifelse(chagos2$taxa == 'Algae', 'macroalgae', chagos2$benthic)

## sum benthic categories 
chagos2$value<-as.numeric(chagos2$value)
chagos2 <- chagos2 %>% group_by(dataset, site, reef, transect, depth, benthic) %>%
			summarise(value = sum(value))
chagos2$value[is.na(chagos2$value)]<-0

## keep only total coral and macroalgae
chagos2 <- chagos2 %>% filter(benthic %in% c('hard.coral', 'macroalgae'))

# change factors to characters
chagos2$site<-as.character(chagos2$site)
chagos2$reef<-as.character(chagos2$reef)
chagos2$transect<-as.character(chagos2$transect)


## merge chagos and chagos2
# drop date from chagos to merge, bring in from fish data
chagos$date<-NULL

## ungroup
chagos<-ungroup(chagos); chagos2<-ungroup(chagos2)
chagos<-rbind(chagos, chagos2)

## add ID
chagos$unique.id<-with(chagos, paste(reef, site, sep='.'))

## average across replicates
chagos <- chagos %>% group_by(dataset, site, reef, benthic, unique.id) %>%
				summarise(cover = mean(value))


## ------------------------------------ ##
		   ## clean GBR ##
## ------------------------------------ ##
## drop 3 NAs
gbr <- gbr[!gbr$Reef=='',]
colnames(gbr)[1:6]<-c('date', 'reef', 'site.number', 'habitat', 'depth', 'transect')

## gather taxa into 1 column
gbr <- gather(gbr, taxa, value, -date, -reef, -site.number, -habitat, -depth, -transect)
gbr$dataset<-'GBR'
gbr$site<-paste0(gbr$reef, gbr$site.number)


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
gbr$unique.id<-with(gbr, paste(site, reef, sep='.'))


gbr <- gbr %>% group_by(dataset, site, reef, benthic, unique.id) %>%
				summarise(cover = mean(value))

## is there one hard.coral and one macroalgae value per site?
with(gbr, table(unique.id, benthic))
## yes


## save individual
save(chagos, gbr, maldives, seychelles, file='data/wio_benthic_master.Rdata')

pred<-herb
## drop seychelles 1994, 2005
pred<-pred %>% filter(!(date %in% c(1994, 2005)))

### fix chagos site names to match with benthos
sites<-read.csv('data/raw-data/csv/chagos_sitenames.csv')
unique(pred$site[pred$dataset=='Chagos'])
with(pred[pred$dataset=='Chagos',], table(site, reef))
unique(chagos$site[chagos$dataset=='Chagos'])
pred$site[pred$site=='Barton Point']<-'Barton Point east'
pred$site[pred$site=='Diego Garcia East coast']<-'East side'
pred$site[pred$site=='Exposed site 1']<-'Petite Coq (E4)'
pred$site[pred$site=='Exposed site 2']<-'South Coq (E5)'
pred$site[pred$site=='Exposed site 3']<-'South Coq (E6)'
pred$site[pred$site=='Ile Fouquet']<-'E3'
pred$site[pred$site=='Ile Poule']<-'Ile Poule (S1)'
pred$site[pred$site=='Ile Takamaka']<-'E2'
pred$site[pred$site=='Middle Brother']<-'Middle Brother (E3)'
pred$site[pred$site=='Sheltered site 2']<-'Ile Poule (S2)'
pred$site[pred$site=='Sheltered site 3']<-'S3'
pred$site[pred$site=='South']<-'E4'

## fix sites which have more info in the unique id
unique(pred$unique.id[pred$site=='South Brother'])
pred$site[pred$unique.id == 'Great Chagos BankSouth BrotherExposed1']<-'South Brother (E1)'
pred$site[pred$unique.id == 'Great Chagos BankSouth BrotherExposed2']<-'South Brother (E2)'

unique(pred$unique.id[pred$site=='Eagle'])
pred$site[pred$unique.id == 'Great Chagos BankEagleSheltered1']<-'Eagle (S1)'
pred$site[pred$unique.id == 'Great Chagos BankEagleSheltered2']<-'Eagle (S2)'
pred$site[pred$unique.id == 'Great Chagos BankEagleSheltered3']<-'Eagle (S3)'


## S3 is labelled for Peuros Banhos and Salamon. Introduce new ID
unique(pred$unique.id[pred$site=='Ile Anglais'])
pred$site[pred$unique.id == 'SalamonIle AnglaisSheltered1']<-'Isle le Anglaise (S1)'
pred$site[pred$unique.id == 'SalamonIle AnglaisSheltered2']<-'S2'
pred$site[pred$unique.id == 'SalamonIle AnglaisSheltered3']<-'Isle le Anglaise (S3)'

## match up in benthic data
chagos$site[grepl('Salamon.S3', chagos$unique.id)]<-'Isle le Anglaise (S3)'
## fix unique.id
chagos$unique.id<-with(chagos, paste(reef, site, sep='.'))


## what is missing?
unique(chagos$site)[!(unique(chagos$site) %in% unique(pred$site[pred$dataset=='Chagos']))]

## any duplicate IDs?
aggregate(unique.id ~ site, pred[pred$dataset=='Chagos',], uniques ) ## no
aggregate(unique.id ~ site, chagos, uniques ) ## 4 transects each. ok.


## add IDs separately for each dataset
pred$unique.id<-as.character(pred$unique.id)
pred$unique.id[pred$dataset == 'Seychelles'] <- with(pred[pred$dataset == 'Seychelles',], paste(date,site, sep='.'))
pred$unique.id[pred$dataset == 'Chagos'] <- with(pred[pred$dataset == 'Chagos',], paste(reef, site, sep='.'))
pred$unique.id[pred$dataset == 'Maldives'] <- with(pred[pred$dataset == 'Maldives',], paste(reef, site, sep='.'))
pred$unique.id[pred$dataset == 'GBR'] <- with(pred[pred$dataset == 'GBR',], paste(site, reef, sep='.'))



## Matching looks horrible but it works. hoo-fucking-rah.

pred$hard.coral[pred$dataset=='Seychelles']<-seychelles$cover[seychelles$benthic=='hard.coral'][match(pred$unique.id[pred$dataset=='Seychelles'], seychelles$unique.id[seychelles$benthic=='hard.coral'])]
pred$hard.coral[pred$dataset=='Maldives']<-maldives$cover[maldives$benthic=='hard.coral'][match(pred$unique.id[pred$dataset=='Maldives'], maldives$unique.id[maldives$benthic=='hard.coral'])]
pred$hard.coral[pred$dataset=='GBR']<-gbr$cover[gbr$benthic=='hard.coral'][match(pred$unique.id[pred$dataset=='GBR'], gbr$unique.id[gbr$benthic=='hard.coral'])]
pred$hard.coral[pred$dataset=='Chagos']<-chagos$cover[chagos$benthic=='hard.coral'][match(pred$unique.id[pred$dataset=='Chagos'], chagos$unique.id[chagos$benthic=='hard.coral'])]

pred$macroalgae[pred$dataset=='Seychelles']<-seychelles$cover[seychelles$benthic=='macroalgae'][match(pred$unique.id[pred$dataset=='Seychelles'], seychelles$unique.id[seychelles$benthic=='macroalgae'])]
pred$macroalgae[pred$dataset=='Maldives']<-maldives$cover[maldives$benthic=='macroalgae'][match(pred$unique.id[pred$dataset=='Maldives'], maldives$unique.id[maldives$benthic=='macroalgae'])]
pred$macroalgae[pred$dataset=='GBR']<-gbr$cover[gbr$benthic=='macroalgae'][match(pred$unique.id[pred$dataset=='GBR'], gbr$unique.id[gbr$benthic=='macroalgae'])]
pred$macroalgae[pred$dataset=='Chagos']<-chagos$cover[chagos$benthic=='macroalgae'][match(pred$unique.id[pred$dataset=='Chagos'], chagos$unique.id[chagos$benthic=='macroalgae'])]


## checking matching works
head(pred[pred$dataset=='Seychelles',])
seychelles %>% filter(unique.id == '2008.Cousin Carbonate')
tail(pred[pred$dataset=='Seychelles',])
seychelles %>% filter(unique.id == '2017.Praslin SW Patch')

head(pred[pred$dataset=='GBR',])
gbr %>% filter(unique.id =='Davies1.Davies')
tail(pred[pred$dataset=='GBR',])
gbr %>% filter(unique.id =='Wheeler3.Wheeler')

head(pred[pred$dataset=='Maldives',])
maldives %>% filter(unique.id =='Maamendhoo.Huvadhoo')
tail(pred[pred$dataset=='Maldives',])
maldives %>% filter(unique.id =='Gemanafushi.Huvadhoo')

head(pred[pred$dataset=='Chagos',])
chagos %>% filter(unique.id =='Great Chagos Bank.Middle Brother (E3)')
tail(pred[pred$dataset=='Chagos',])
chagos %>% filter(unique.id =='Diego Garcia.East side')

## any NA benthos?
pred %>% filter(is.na(hard.coral))
pred %>% filter(is.na(macroalgae))

## yes macroalgae at one site = Maamendhoo.Huvadhoo. Not observed.
pred$macroalgae[is.na(pred$macroalgae)]<-0


### NOW ADD STRUCTURAL COMPLEXITY
load('data/wio_complexity.Rdata')

## merge seychelles complex
seychelles.complex<-seychelles.complex %>% group_by(dataset, site, transect, unique.id) %>%
			summarise(complexity = mean(value))

complex<-rbind(complex, seychelles.complex)

## match into pred
pred$complexity<-complex$complexity[match(pred$unique.id, complex$unique.id)]
## any NAs?
dim(pred[is.na(pred$complexity),]) # NOPE



## final data tidying - add dates for chagos and switch to years for other sites
chagos12sites<-c('Barton Point east', 'Barton Point west', 'Cannon Point', 'Cannon Point 2', 'East side', 'Middle Island', "North Diamont")
pred$date[pred$dataset=='Chagos']<-ifelse(pred$reef[pred$dataset=='Chagos'] %in% chagos12sites, "2012", "2010")

pred$date<-str_replace_all(pred$date, 'Nov ', '')
pred$date<-str_replace_all(pred$date, 'Jan ', '')
pred$date<-str_replace_all(pred$date, '11/03/', '')
pred$date<-str_replace_all(pred$date, '12/03/', '')
pred$date<-str_replace_all(pred$date, '13/03/', '')
pred$date<-str_replace_all(pred$date, '14/03/', '')
pred$date<-str_replace_all(pred$date, '15/03/', '')


## some extra stuff we found during model process
pred$depth[pred$dataset=='Seychelles']<-10
pred$depth[pred$dataset=='GBR']<-10
# drop 3m sites in Chagos with LUDACRIS/Luka Modric biomass
pred<-pred %>% filter(depth != 3) 


## save master
save(pred, file='data/wio_herb_benthic_merged.Rdata')




