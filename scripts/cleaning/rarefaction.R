
library(tidyverse)
library(funk)
library(scales)
library(iNEXT)
library(here)
theme_set(theme_sleek())
setwd(here('grazing-gradients'))


# data load
load("data/wio_herb_benthic_merged.Rdata")
h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum abund per FG in each transect
        group_by(unique.id, species) %>%
          summarise(abund = length(abundance.500m2)) #%>%
  ## mean species abundass across transects at each site
          # group_by(unique.id, species) %>%
          # summarise(abund = round(mean(abund)))

samples<-pred %>% filter(FG == 'Herbivore Scraper') %>% 
  group_by(unique.id) %>%
  summarise(samples = uniques(transect)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, abund)
# com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -c(1)]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format

com.mat<-as.matrix(com.mat)
dim(com.mat)
## transpose
com.mat<-t(com.mat)
colnames(com.mat)<-rows$unique.id

rownames(com.mat)<-NULL
com.mat<-rbind(samples$samples, com.mat)
temp<-apply(com.mat,2, as.list)
temp<-lapply(temp,unlist)
t<-estimateD(temp, datatype='abundance', base='size', level = NULL)
t<-t %>% filter(order == 0)

write.csv(t, file = 'results/rarefied_richness_scrapers.csv')


## repeat for croppers
# data load
load("data/wio_herb_benthic_merged.Rdata")
h <- pred %>% filter(FG == 'Herbivore Grazer') %>% 
  ## sum abund per FG in each transect
        group_by(unique.id, species) %>%
          summarise(abund = length(abundance.500m2)) #%>%
  ## mean species abundass across transects at each site
          # group_by(unique.id, species) %>%
          # summarise(abund = round(mean(abund)))

samples<-pred %>% filter(FG == 'Herbivore Grazer') %>% 
  group_by(unique.id) %>%
  summarise(samples = uniques(transect)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, abund)
# com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -c(1)]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format

com.mat<-as.matrix(com.mat)
dim(com.mat)
## transpose
com.mat<-t(com.mat)
colnames(com.mat)<-rows$unique.id

rownames(com.mat)<-NULL
com.mat<-rbind(samples$samples, com.mat)
temp<-apply(com.mat,2, as.list)
temp<-lapply(temp,unlist)
t<-estimateD(temp, datatype='abundance', base='coverage', level = NULL)
t<-t %>% filter(order == 0)

write.csv(t, file = 'results/rarefied_richness_croppers.csv')


