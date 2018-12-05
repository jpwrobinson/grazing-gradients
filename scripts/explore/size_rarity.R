
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
load('results/models/scraper_function.Rdata')
h <- h %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha), scraping=sum(scraping)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, species) %>%
          summarise(biom = mean(biom), scraping=mean(scraping)) 



## change names for colnames
com.mat<-tidyr::spread(h, species, biom)
# com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -c(1)]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format
com.mat<-as.matrix(com.mat)
dim(com.mat)


## estimate diversity
library(vegan)
div<-data.frame(div=diversity(com.mat), 
				richness=specnumber(com.mat), 
				unique.id = rows)
div$J <- div$div/log(div$richness)
# save mean sizes 
sizes<-pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, species,
                 unique.id) %>%
          summarise(size = mean(length.cm), mass= mean(mass.g)) %>%
  ## mean species sizeass across transects at each site
          group_by(species) %>%
          summarise(size = mean(size), mass=mean(mass)) 

com.mat.inc<-com.mat
com.mat.inc[com.mat.inc>0]<-1
freq<-data.frame(freq=colSums(com.mat.inc), species = colnames(com.mat.inc))
freq$biom<-h$biom[match(freq$species, h$species)]
freq$size.cm<-sizes$size[match(freq$species, sizes$species)]
freq$size.g<-sizes$mass[match(freq$species, sizes$species)]


ggplot(freq, aes(reorder(species,freq), freq)) + geom_bar(stat='identity') + coord_flip()
ggplot(freq, aes(size.g, freq, size=biom))  + scale_x_log10() + geom_text(aes(label=species))