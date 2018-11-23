
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)

setwd(here('grazing-gradients'))



## biomass models and predictions
load("results/models/biomass_m.browsers.Rdata")
load("results/models/biomass_m.scrapers.Rdata")
load("results/models/biomass_m.grazers.Rdata")
load("results/models/biomass_m.predictions.Rdata")

rsquared(m.browser)
#       Response   family     link method  Marginal Conditional
# 1 browserlog10 gaussian identity   none 0.2232465    0.347809

rsquared(m.grazer)
#      Response   family     link method  Marginal Conditional
# 1 grazerlog10 gaussian identity   none 0.2646925   0.6860404

rsquared(m.scraper)
#       Response   family     link method Marginal Conditional
# 1 scraperlog10 gaussian identity   none  0.41861   0.5426255

## decoupling models and predictions
load("results/models/browser_function.Rdata")
load("results/models/scraper_function.Rdata")
load("results/models/cropper_function.Rdata")
rsquared(m.browse)
#   Response   family     link method  Marginal Conditional
# 1 browsing gaussian identity   none 0.9979434   0.9979434

rsquared(m.graze)
#           Response   family     link method  Marginal Conditional
# 1 cropping.gram.ha gaussian identity   none 0.9466518   0.9567969


rsquared(m.scrape)
#   Response   family     link method  Marginal Conditional
# 1 scraping gaussian identity   none 0.4094991    0.759337

## scraping models
load("results/models/scraping_model.Rdata")
rsquared(m.scraper)
# Marginal Conditional
#  0.1459465   0.6475017
 

## raw dataset
load("data/wio_herb_benthic_merged.Rdata")

## assign seychelles 2017 with mean complexity values for now - needs fixed
pred$complexity[pred$dataset == 'Seychelles' & pred$date == 2017] <- mean(pred$complexity)

# estimate mean biomass per site per FG
h <- pred %>% 
  ## sum biomass per FG in each transect
  group_by(dataset, date, reef, site, management, transect, 
           unique.id, depth, FG,
           hard.coral, macroalgae, rubble, substrate, complexity, fish.biom) %>%
  summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
  group_by(dataset, date, reef, site, management, unique.id, depth, FG,
           hard.coral, macroalgae,  rubble, substrate, complexity, fish.biom) %>%
  summarise(biom = mean(biom)) 


## function models and data

## cropper = algal consumption
load("results/models/cropper_function.Rdata")
grazers<-h
grazers$grazef<-grazers$cropping.gram.ha
grazers$cropping.gram.ha<-NULL
grazers$sp <- 'grazers'

m.graze<-lmer(cropping.gram.ha ~ biom + (1 | dataset), h)
grazers$resid<-resid(m.graze)
r2marg.grazer<-rsquared(m.graze)$Marginal

## browser - mass standardized bite rates
load("results/models/browser_function.Rdata")
browsers<-h
browsers$grazef<-browsers$browsing
browsers$browsing<-NULL
browsers$sp <- 'browsers'

m.browse<-lmer(browsing ~ biom + (1 | dataset), h)
browsers$resid<-resid(m.browse)
r2marg.browser<-rsquared(m.browse)$Marginal

## doesn't exist yet
# load("results/models/scraper_function.Rdata")