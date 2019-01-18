
library(here)
library(tidyverse)
setwd(here('grazing-gradients/'))
theme_set(theme_bw())

# UVC data load
load("data/wio_gbr_fish_master.Rdata")
sey<-herb %>% filter(FG == 'Herbivore Scraper' & dataset=='Seychelles')
sey$species<-as.factor(sey$sp)
## add genera column
sey$genus<-str_split_fixed(sey$species, pattern = '\\ ', n=2)[,1]
sey$genus<-as.factor(sey$genus)

## predictive model load
load(file = 'results/models/bites_scrapers.Rdata')

## bite predictions for each species
d.pred <-data.frame(sp = sey$species, Genus = sey$genus, TL=sey$length.cm, dataset='NA')
## subset for species in bite rate obs
d.pred <- d.pred[d.pred$sp %in% scrapers$sp,]; d.pred<-droplevels(d.pred)
a.dataset.zero = matrix(0, 1000, 3)
link.obs.sp<-link(scrape.m, n = 1000, data = as.list(d.pred), replace=list(X3 = a.dataset.zero))
sey$biterate[sey$sp %in% scrapers$sp]<-apply(link.obs.sp, 2, median)

## uncertainty
pred.PI<-apply(link.obs.sp, 2, PI, prob = 0.95)
sey$upper[sey$sp %in% scrapers$sp]<-pred.PI[2,]
sey$lower[sey$sp %in% scrapers$sp]<-pred.PI[1,]

## how much biomass and abunance, and how many species, require genus level predictions?
sum(sey$biomass.kgha[is.na(sey$biterate)])/sum(sey$biomass.kgha)*100 ### 25%
sum(sey$abundance.500m2[is.na(sey$biterate)])/sum(sey$abundance.500m2)*100 ### 27%

## assign genus for missing species
scraper.bites<-read.csv(file = 'results/functions/scraper_bites_predicted.csv')
sey$biterate[is.na(sey$biterate)]<-scraper.bites$median[match(sey$genus[is.na(sey$biterate)], scraper.bites$class)]
  

## area predictions for body size
load(file = 'results/models/area_scrapers.Rdata')
d.pred <-data.frame(TL=sey$length.cm)
link.obs.sp<-link(scrape.m2, n = 1000, data = as.list(d.pred))
sey$bitearea<-data.frame(median = apply(link.obs.sp, 2, median))
pred.PI<-apply(link.obs.sp, 2, PI, prob = 0.95)
sey$upperarea<-pred.PI[2,]
sey$lowerarea<-pred.PI[1,]


## what proportion biomass is global means?
newsp<-unique(sey$species)[!unique(sey$species) %in% scraper.bites$class] ## new species = 5

## how much biomass is global mean in analysis?
t<- sey 
t$id <- ifelse(t$species %in% newsp, 'Genera', 'Species')
tot<-sum(sey$biomass.kgha)
t %>% group_by(id) %>% summarise(b = sum(biomass.kgha)/tot*100)

# Scraper function = area scraped * bite rate, 
# where area scraped is predicted by individual body size and 
# bite rate is predicted by individual body size, species and genus. 
# For units, we have area scraped in mm^2^, bite rate in bites per minute, and biomass in kg per hectare. 
# After converting to area scraped per minute per hectare, millimetre areas are obviously very large for a per hectare biomass estimate. 
# Converting mm^2^ to m^2^ requires dividing by 1,000,000.

## now convert bite rate to area scraped
sey$scraping<-sey$biterate * sey$bitearea / 1000000

# estimate mean total scraping function per site
h <- sey %>% 
  ## sum scraping in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG) %>%
          summarise(scraping = sum(scraping), biom=sum(biomass.kgha), abund = sum(abundance.500m2)) %>%
  ## mean scraping across transects at each site
          group_by(dataset, date, reef, site, management, depth, FG) %>%
          summarise(scraping = mean(scraping), biom=mean(biom), abund = mean(abund)) 

## correct scraping to per hectare
## for surveys = 153.9m2, which is when abundance.500m2 = 3.24806 (Seychelles)
h$scraping <- h$scraping/0.01539 

## drop annoying dplyr things
sey$bitearea<-sey$bitearea[,1]
sey$scraping<-sey$scraping[,1]
save(sey, h, file = 'results/bleaching/scraper_function_seychelles.Rdata')


