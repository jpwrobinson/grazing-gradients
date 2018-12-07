
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)
theme_set(theme_sleek())
setwd(here('grazing-gradients'))


# data load
load("data/wio_herb_benthic_merged.Rdata")
# estimate mean biomass per site per FG
h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha), abund = sum(abundance.500m2)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, species) %>%
          summarise(biom = mean(biom), abund = mean (abund)) 

# estimate mean biomass per site per FG
h.sp <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean species biomass across transects at each site
          group_by(species) %>%
          summarise(biom = mean(biom)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, biom)
# com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -c(1,2)]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format
com.mat<-as.matrix(com.mat)
dim(com.mat)

## species biomass, including zeroees
h.sp2<-data.frame(biom=colMeans(com.mat), species= colnames(com.mat))


## estimate diversity
library(vegan)
div<-data.frame(div=diversity(com.mat), 
				richness=specnumber(com.mat), 
				unique.id = rows)
div$J <- div$div/log(div$richness)


com.mat.inc<-com.mat
com.mat.inc[com.mat.inc>0]<-1
freq<-data.frame(freq=colSums(com.mat.inc), species = colnames(com.mat.inc))

ggplot(freq, aes(reorder(species,freq), freq)) + geom_bar(stat='identity') + coord_flip()

## scraper bite rates
p<-read.csv(file = 'results/functions/scraper_bites_predicted.csv')
freq$bite.rate<-p$median[match(freq$species, p$class)]
freq$biom<-h.sp$biom[match(freq$species, h.sp$species)]
freq$biom2<-h.sp2$biom[match(freq$species, h.sp2$species)]
freq$genus<-str_split_fixed(freq$species, '\ ', 2)[,1]
freq$bite.rate[is.na(freq$bite.rate)]<-p$median[match(freq$genus[is.na(freq$bite.rate)], p$class)]

## check biogeogr - add nregions to freq
regions<-read.csv(file='writing/ms/TableS1_Specieslist.csv')
freq$nregions<-regions$nregions[match(freq$species, regions$species)]

ggplot(freq, aes(bite.rate, freq, label=species)) + geom_text()
ggplot(freq, aes(bite.rate, freq, label=species, size=biom)) + geom_text()
ggplot(freq, aes(bite.rate, freq, label=species, size=biom2)) + geom_text()
ggplot(freq, aes(bite.rate, biom, label=species, col=freq)) + geom_text() + scale_y_log10()+ scale_colour_gradient(low='red', high='green')
ggplot(freq, aes(freq, biom, label=species, col=bite.rate)) + geom_text() + scale_y_log10() + scale_colour_gradient(low='red', high='green')
ggplot(freq, aes(nregions, bite.rate, label=species, col=freq, size=biom)) + geom_text()  + scale_colour_gradient(low='red', high='green')