
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

m.full<-glmer(cropping.gram.ha ~  hard.coral + macroalgae + rubble + substrate + complexity + 
        	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
summary(m.)
## save AIC scores from top 7 models
m.table<-dredge(m.full)
tab<-subset(m.table, delta < 7)
tab<-data.frame(tab)
tab[is.na(tab)]<-0
#recalc model weights for the top model set
top.weights <- tab$weight/sum(m.table$weight[1:dim(tab)[1]])
tab$weight<-top.weights
write.csv(tab, 'results/tables/croppers_AICtable.csv')

## estimated weight t values and predictions
mm.crop<-mmi_tvalue(m.full, dataset=h.pred, t.subset=TRUE, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 'site.size' ), 
		 ranef = c('dataset', 'reef'), indicator = 'cropping.gram.ha', family = 'Gamma')
save(mm.crop, file = 'results/models/tvalues_croppers.Rdata')



# ## repeat with rarefied richness estimate
# mm.crop<-mmi_tvalue(m.full, dataset=h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
#           'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy',  'site.size' ),
#             ranef = c('dataset', 'reef'), indicator = 'cropping.gram.ha', family = 'Gamma')
# save(mm.crop, file = 'results/models/tvalues_croppers_rarefied.Rdata')


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

m.full<-glmer(scraping ~ hard.coral + macroalgae + rubble + substrate + complexity + 
        	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)
data.frame(r2beta(m.full, method = 'nsj', partial = TRUE))

## save AIC scores from top 7 models
m.table<-dredge(m.full)
tab<-subset(m.table, delta < 7)
tab<-data.frame(tab)
tab[is.na(tab)]<-0
#recalc model weights for the top model set
top.weights <- tab$weight/sum(m.table$weight[1:dim(tab)[1]])
tab$weight<-top.weights
write.csv(tab, 'results/tables/scrapers_AICtable.csv')


## estimated weight t values and predictions
mm.scrape<-mmi_tvalue(m.full, dataset=h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 'site.size' ), 
		 ranef = c('dataset', 'reef'), indicator = 'scraping', family = 'Gamma')
save(mm.scrape, file = 'results/models/tvalues_scrapers.Rdata')

# mm.scrape<-mmi_tvalue(m.full, dataset=h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
#           'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 'site.rarefied', 'site.size' ),
#             ranef = c('dataset', 'reef'), indicator = 'scraping', family = 'Gamma')
# save(mm.scrape, file = 'results/models/tvalues_scrapers_rarefied.Rdata')
