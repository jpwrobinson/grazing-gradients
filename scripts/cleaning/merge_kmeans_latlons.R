
library(tidyverse)
library(funk)
library(here)

setwd(here('grazing-gradients'))

#------------------------#
#------ data load ------#
#------------------------#
# biomass
load("data/wio_herb_benthic_merged.Rdata")

# benthic regimes 
load("data/pca_allregions_kmeans3_clusters.Rdata") # pca.kmeans
pca.kmeans$site<-rownames(pca.kmeans)
pca.kmeans$dataset<-pred$dataset[match(pca.kmeans$site, pred$unique.id)]
pca.kmeans$date<-pred$date[match(pca.kmeans$site, pred$unique.id)]
pca.kmeans$site2<-pred$site[match(pca.kmeans$site, pred$unique.id)]

## fix maldives
pca.kmeans$site2[pca.kmeans$dataset=='Maldives']<-str_split_fixed(pca.kmeans$site[pca.kmeans$dataset=='Maldives'], '\\.', 2)[,1]

## fix seychelles
## seychelles has multiple years, but regimes do not change over time
aggregate(km.cluster ~ site, pca.kmeans[pca.kmeans$dataset=='Seychelles',], uniques)


## use 2017 seychelles as representative
pca.kmeans<-pca.kmeans[!(pca.kmeans$date %in% c(2008, 2011, 2017) & pca.kmeans$dataset=='Seychelles'),]


## lat lons
sites<-read.csv('data/grazing-site-latlon.csv')
## need to convert to decimal degrees
sites$Y_direction<-as.factor(with(sites, ifelse(Y < 0, 'S', 'N')))
sites$Y<-with(sites, ifelse(Y < 0, Y*-1, Y))
sites$Y_d<-str_split_fixed(sites$Y, '\\.', 2)[,1]
sites$Y_min<-str_split_fixed(sites$Y, '\\.', 2)[,2]
sites$Y_min<-ifelse(nchar(sites$Y_min) == 4, paste0(sites$Y_min,0), sites$Y_min)
sites$Y_min<-ifelse(nchar(sites$Y_min) == 3, paste0(sites$Y_min,00), sites$Y_min)
sites$Y_sec<-substring(sites$Y_min, 3, 5)
sites$Y_sec<-paste(substring(sites$Y_min, 1,2), substring(sites$Y_min, 2,3), sep='.')
sites$Y_min<-substring(sites$Y_min, 1, 2)

sites$X_d<-str_split_fixed(sites$X, '\\.', 2)[,1]
sites$X_min<-str_split_fixed(sites$X, '\\.', 2)[,2]
sites$X_min<-ifelse(nchar(sites$X_min) == 4, paste0(sites$X_min,0), sites$X_min)
sites$X_min<-ifelse(nchar(sites$X_min) == 3, paste0(sites$X_min,00), sites$X_min)
sites$X_sec<-substring(sites$X_min, 3, 5)
sites$X_sec<-paste(substring(sites$X_min, 1,2), substring(sites$X_min, 2,3), sep='.')
sites$X_min<-substring(sites$X_min, 1, 2)

## change characters to numeric, calculate degrees 
sites<-sites %>% mutate_if(is.character, as.numeric)
sites$Y<-with(sites, Y_d + (Y_min/60) + (Y_sec/3600))
sites$X<-with(sites, X_d + (X_min/60) + (X_sec/3600))

# change Y back to negative
sites$Y<-with(sites, ifelse(Y_direction == 'S', Y * -1, Y))


## drop deg, min, and merge seychelles
sites <- sites[, -c(7:13)]
seychelles<-read.csv('data/sey_latlon.csv')
sites<-rbind(sites, seychelles)

## clean sites to match up
sites$Site<-str_replace_all(sites$Site, 'Vilingi', 'Vilingili')
sites$Site[sites$dataset=='GBR']<-paste0(sites$Reef[sites$dataset=='GBR'], sites$Unique_site_transect[sites$dataset=='GBR'])
sites$Site<-str_replace_all(sites$Site, ' carbonate', ' Carbonate')
sites$Site<-str_replace_all(sites$Site, ' granite', ' Granite')
sites$Site<-str_replace_all(sites$Site, ' patch', ' Patch')
sites$Site<-str_replace_all(sites$Site, ' granitic', ' Granite')
sites$Site<-str_replace_all(sites$Site, ' reef', '')
sites$Site<-str_replace_all(sites$Site, 'Ste. Anne', 'Ste Anne')

## match in clusters
sites$km.cluster<-pca.kmeans$km.cluster[match(sites$Site, pca.kmeans$site2)]
sites$km.cluster[is.na(sites$km.cluster)]<-pca.kmeans$km.cluster[match(sites$Site[is.na(sites$km.cluster)], pca.kmeans$site)]


## save
write.csv(sites, file = 'data/sites_with_benthic_clusters.csv')