
library(here)
setwd(here('grazing-gradients'))

library(piecewiseSEM)
require(gridExtra)
library(grid)
library(lme4)
#library(sjPlot)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)

## Script fits multiple models to cropper/scraper functions, measures t-values and weighted model preds

## croppers
load(file = 'results/models/cropper_function.Rdata')
h$management<-factor(h$management)
load(file = 'results/cropper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]
rare<-read.csv(file = 'results/rarefied_richness_croppers.csv')
h$site.rarefied<-rare$qD[match(h$unique.id, rare$site)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))


mm.crop<-mmi_tvalue(h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 
          'site.richness', 'site.size' ), indicator = 'cropping.gram.ha', family = 'Gamma')
save(mm.crop, file = 'results/models/tvalues_croppers.Rdata')

## repeat with rarefied richness estimate
mm.crop<-mmi_tvalue(h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 
          'site.rarefied', 'site.size' ), indicator = 'cropping.gram.ha', family = 'Gamma')
save(mm.crop, file = 'results/models/tvalues_croppers_rarefied.Rdata')


## now scrapers
load(file = 'results/models/scraper_function.Rdata')
h$management<-factor(h$management)
load(file = 'results/scraper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]
rare<-read.csv(file = 'results/rarefied_richness_scrapers.csv')
h$site.rarefied<-rare$qD[match(h$unique.id, rare$site)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'scraping'))


mm.scrape<-mmi_tvalue(h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 
          'site.richness', 'site.size' ), indicator = 'scraping', family = 'Gamma')
save(mm.scrape, file = 'results/models/tvalues_scrapers.Rdata')

mm.scrape<-mmi_tvalue(h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 
          'site.rarefied', 'site.size' ), indicator = 'scraping', family = 'Gamma')
save(mm.scrape, file = 'results/models/tvalues_scrapers_rarefied.Rdata')
