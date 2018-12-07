
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
pdf(file='figures/explore/scraping_rarity.pdf', height= 7, width=12)

# data load
load('results/models/scraper_function_species.Rdata')
scrape <- h.sp


rsr<-data.frame(rsr); colnames(rsr)<-c('mean.richness', 'species')
rsr$mean.richness<-as.numeric(as.character(rsr$mean.richness))

## match in species level predictors/info
scrape$mean.species.biom<-h$biom[match(scrape$species, h$species)]
scrape$mean.richness<-rsr$mean.richness[match(scrape$species, rsr$species)]
scrape$mean.species.size.cm<-sizes.sp$size[match(scrape$species, sizes.sp$species)]

p<-read.csv(file = 'results/functions/scraper_bites_predicted.csv')
scrape$genus<-str_split_fixed(scrape$species, '\ ', 2)[,1]
scrape$mean.bite.rate<-p$median[match(scrape$species, p$class)]
scrape$mean.bite.rate[is.na(scrape$bite.rate)]<-p$median[match(scrape$genus[is.na(scrape$bite.rate)], p$class)]
scrape$genus<-NULL


## add site richness to h dataframe
scrape$richness<-div$richness[match(scrape$unique.id, div$unique.id)]
scrape$size<-sizes$size[match(scrape$unique.id, sizes$unique.id)]
ggplot(scrape, aes(richness, scraping, size=biom)) + geom_point() + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'area scraped', title='Species scraping area by assemblage richness')
