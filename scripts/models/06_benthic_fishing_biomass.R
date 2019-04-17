
## Script to fit models to function patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr); library(funk); library('rms')
theme_set(theme_sleek())
library(piecewiseSEM); library(sjPlot); library(r2glmm)
source('scripts/functions/mmi_tvalue_simple.R')

## cropper functions
load(file = 'results/models/cropper_function.Rdata')
h$management<-factor(h$management)

load(file = 'results/cropper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]

rare<-read.csv(file = 'results/rarefied_richness_croppers.csv')
h$site.rarefied<-rare$qD[match(h$unique.id, rare$site)]

## add beta estimates
load('results/graze_beta_estimates.Rdata')
h$site.beta<-s.diversity$beta[match(h$unique.id, s.diversity$site)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'biom', 'site.rarefied', 'site.beta'))
summary(h.pred)


## cropper BIOMASS
m.full<-glmer(biom ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)

# rcrop<-data.frame(r2beta(m.full, method = 'nsj', partial = TRUE))
# rcrop$response<-'biom'

## estimated weight t values and predictions
mm<-mmi_tvalue_simp(m.full, dataset=h.pred, t.subset=TRUE, indicator = 'biom')
mm$class<-ifelse(mm$Var %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
mm$response<-'biom'
save(mm, file = 'results/models/tvalues_croppers_biom.Rdata')


## cropper RAREFIED
m.full<-lmer(site.rarefied ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, na.action = na.fail)

# t<-data.frame(r2beta(m.full, method = 'nsj', partial = TRUE)); t$response <- 'site.rarefied'
# rcrop<-rbind(rcrop, t)

## estimated weight t values and predictions
mm<-mmi_tvalue_simp(m.full, dataset=h.pred, t.subset=TRUE, indicator = 'site.rarefied')
mm$class<-ifelse(mm$Var %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
mm$response<-'richness'
save(mm, file = 'results/models/tvalues_croppers_site.rarefied.Rdata')


## cropper BETA
m.full<-glmer(site.beta ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)

# t<-data.frame(r2beta(m.full, method = 'nsj', partial = TRUE)); t$response <- 'site.beta'
# rcrop<-rbind(rcrop, t)

# ## estimated weight t values and predictions
mm<-mmi_tvalue_simp(m.full, dataset=h.pred, t.subset=TRUE, indicator = 'site.beta')
mm$class<-ifelse(mm$Var %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
mm$response<-'beta'
save(mm, file = 'results/models/tvalues_croppers_site.beta.Rdata')


## scraper functions
load(file = 'results/models/scraper_function.Rdata')
h$management<-factor(h$management)

load(file = 'results/scraper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]

rare<-read.csv(file = 'results/rarefied_richness_scrapers.csv')
h$site.rarefied<-rare$qD[match(h$unique.id, rare$site)]

## add beta estimates
load('results/graze_beta_estimates.Rdata')
h$site.beta<-s.diversity$beta[match(h$unique.id, s.diversity$site)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id','scraping', 'biom', 'site.rarefied', 'site.beta', 'abund'))
summary(h.pred)

## check collinearity
mat<-h.pred %>% select(hard.coral, macroalgae, rubble, substrate, complexity, 
          fish.biom, Fished.Protected.dummy, Fished.Unfished.dummy, site.size)
pairs2(mat, diag.panel = panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth2)

## scraper BIOMASS
m.full<-glmer(biom ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + #site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)
summary(m.full)

# rscrap<-data.frame(r2beta(m.full, method = 'nsj', partial = TRUE))
# rscrap$response<-'biom'

# estimated weight t values and predictions
mm<-mmi_tvalue_simp(m.full, dataset=h.pred, t.subset=TRUE, indicator = 'biom')
mm$class<-ifelse(mm$Var %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
mm$response<-'biom'
save(mm, file = 'results/models/tvalues_scrapers_biom.Rdata')


## scraper RAREFIED
m.full<-lmer(site.rarefied ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, na.action = na.fail)
summary(m.full)
# t<-data.frame(r2beta(m.full, method = 'nsj', partial = TRUE)); t$response <- 'site.rarefied'
# rscrap<-rbind(rscrap, t)

## estimated weight t values and predictions
mm<-mmi_tvalue_simp(m.full, dataset=h.pred, t.subset=TRUE, indicator = 'site.rarefied')
mm$class<-ifelse(mm$Var %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
mm$response<-'richness'
save(mm, file = 'results/models/tvalues_scrapers_site.rarefied.Rdata')


## scraper BETA
m.full<-glmer(site.beta ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)

# t<-data.frame(r2beta(m.full, method = 'nsj', partial = TRUE)); t$response <- 'site.beta'
# rscrap<-rbind(rscrap, t)

## estimated weight t values and predictions
mm<-mmi_tvalue_simp(m.full, dataset=h.pred, t.subset=TRUE, indicator = 'site.beta')
mm$class<-ifelse(mm$Var %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
mm$response<-'beta'
save(mm, file = 'results/models/tvalues_scrapers_site.beta.Rdata')



# save(rcrop,rscrap, file = 'results/models/rsq_partial.Rdata')