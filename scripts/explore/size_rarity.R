
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
# pdf(file='figures/explore/scraping_rarity.pdf', height= 7, width=12)

# data load
load('results/models/scraper_function_species.Rdata')
scrape <- h.sp

scrape.prop<-scrape %>% group_by(unique.id) %>% 
				mutate(total = sum(scraping)) %>%
				group_by(species, unique.id) %>%
				mutate(prop = scraping / total) %>%
				group_by(species) %>%
				summarise(scrape.contribution = mean(prop))

scrape.sp<-scrape %>% group_by(species) %>% 
				summarise(scraping = mean(scraping))

load("data/wio_herb_benthic_merged.Rdata")
# estimate mean biomass per site per FG
h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, species) %>%
          summarise(biom = mean(biom)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, biom)
rows<-com.mat[,1]
com.mat<-com.mat[, -c(1)]
com.mat[is.na(com.mat)]<-0
com.mat<-as.matrix(com.mat)

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
          summarise(size = mean(length.cm), mass= mean(mass.g))
  ## mean species sizes across transects at each site
 sizes.sp<- sizes %>%   group_by(species) %>%
          summarise(size = mean(size), mass=mean(mass)) 

## create dataframe of species-level metrics - size, scraping, frequency
com.mat.inc<-com.mat
com.mat.inc[com.mat.inc>0]<-1
freq<-data.frame(freq=colSums(com.mat.inc), species = colnames(com.mat.inc))
freq$biom<-h$biom[match(freq$species, h$species)]
freq$size.cm<-sizes.sp$size[match(freq$species, sizes.sp$species)]
freq$size.g<-sizes.sp$mass[match(freq$species, sizes.sp$species)]
freq$scrape.prop<-scrape.prop$scrape.contribution[match(freq$species, scrape.prop$species)]
freq$scraping<-scrape.sp$scraping[match(freq$species, scrape.sp$species)]

## add site richness to h dataframe
scrape$richness<-div$richness[match(scrape$unique.id, div$unique.id)]
scrape$size<-sizes$size[match(scrape$unique.id, sizes$unique.id)]
ggplot(scrape, aes(richness, scraping, size=biom)) + geom_point() + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'area scraped', title='Species scraping area by assemblage richness')


ggplot(scrape, aes(richness, size)) + geom_point() + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'mean size cm', title='Species average size by assemblage richness')


ggplot(freq, aes(reorder(species,freq), freq)) + geom_bar(stat='identity') + coord_flip()
ggplot(freq, aes(size.g, freq, size=biom))  + scale_x_log10() + geom_text(aes(label=species))

## large species have greater average contribution to scraping 
ggplot(freq, aes(size.g, scrape.prop, size=freq))  +
		 scale_x_log10() + 
		 geom_text(aes(label=species)) + stat_smooth(method = 'lm') +
		 labs(y = 'mean proportion of scraping per site', title='Species contribution to scraping by average size')

ggplot(freq, aes(size.g, scraping, size=freq))  +
		 scale_x_log10() + 
		 geom_text(aes(label=species)) + stat_smooth(method = 'lm') +
		 labs(y = 'mean scraped area per site', title='Species scraping by average size')

## calculate LFI for each site
lfi <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
	mutate(large = ifelse(length.cm > 35, 'large', 'small')) %>%
	 group_by(dataset, reef, site, transect, 
                 unique.id) %>%
          mutate(biom = sum(biomass.kgha)) %>%
          group_by(dataset, reef, site, transect, 
                 unique.id, large) %>%
          summarise(lfi = sum(biomass.kgha)/unique(biom)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, large) %>%
          summarise(lfi = mean(lfi))  %>% filter(large == 'large')

### compare with richness
div$lfi<-lfi$lfi[match(div$unique.id, lfi$unique.id)]
div$lfi[is.na(div$lfi)]<-0

ggplot(div, aes(richness, lfi)) + geom_point() + stat_smooth(method = 'lm') +
		 labs(y = 'biomass proportion of fish > 35cm', title='proportion excavators by species richness')

ggplot(div, aes( lfi, scraping, size=richness)) + geom_point() + stat_smooth(method = 'lm') +
		 labs(y = 'scraped area',x ='biomass proportion fish > 35cm', title='area scraped by proportion excavators')


# dev.off()

load(file = 'results/models/scraper_richness_size_effects.Rdata')
div$scraping<-h$scraping[match(div$unique.id, h$unique.id	)]

ggplot(div, aes(richness, scraping, size=lfi)) + geom_point() + stat_smooth(method = 'lm') +
		 labs(y = 'scraped area', title='scraping by richness with LFI')

