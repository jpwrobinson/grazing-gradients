
## Script to fit models to biomass patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr); library(funk); library('rms')
theme_set(theme_sleek())

# data load
load("data/wio_herb_benthic_merged.Rdata")


# estimate mean biomass per site per FG
h <- pred %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG,
                         hard.coral, macroalgae, complexity) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG,
                         hard.coral, macroalgae, complexity) %>%
          summarise(biom = mean(biom)) 


h<- spread(h, FG, biom, fill=0)
colnames(h)[11:13]<-c('browser', 'grazer', 'scraper')
h<-as.data.frame(h)

# pairs2(data.frame(h$hard.coral, h$macroalgae, h$complexity, h$management), 
#        lower.panel=panel.smooth2, upper.panel = panel.cor)

##scale vars
# h$hard.coral <- scale(h$hard.coral)
# h$macroalgae <- scale(h$macroalgae)
# h$complexity <- scale(h$complexity)
h$management<-factor(h$management)
h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'grazer', 'scraper', 'browser'))

## convert biomass to log10
h.pred$grazerlog10<-log10(h.pred$grazer+1)
h.pred$scraperlog10<-log10(h.pred$scraper+1)
h.pred$browserlog10<-log10(h.pred$browser+1)

## !!! These models don't have random effects !!!

## GAMs supported over GLMs for full model
exp.names = c('macroalgae','hard.coral',  'complexity', 'management')
glm_gam_test(h, exp.names = exp.names, 'grazerlog10', family='gaussian')
glm_gam_test(h, exp.names = exp.names, 'scraperlog10', family='gaussian')
glm_gam_test(h, exp.names = exp.names, 'browserlog10', family='gaussian')


mmi_tvalue(h, exp.names = exp.names, 'grazerlog10', family='gaussian') ## hard coral strongest covariate
mmi_tvalue(h, exp.names = exp.names, 'scraperlog10', family='gaussian') ##  hard coral is strongest covariate
mmi_tvalue(h, exp.names = exp.names, 'browserlog10', family='gaussian') ## macroalgae is strongest covariate

