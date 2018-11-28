
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
load("data/pca_allregions_kmeans4_clusters.Rdata") # pca.kmeans
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