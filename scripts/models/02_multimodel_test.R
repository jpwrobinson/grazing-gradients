
## Script to fit models to biomass patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr); library(funk); library('rms')
theme_set(theme_sleek())
library(piecewiseSEM); library(sjPlot)
# data load
load("data/wio_herb_benthic_merged.Rdata")


# estimate mean biomass per site per FG
h <- pred %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG,
                         hard.coral, macroalgae, complexity, rubble, substrate, fish.biom) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG,
                         hard.coral, macroalgae, complexity, rubble, substrate, fish.biom) %>%
          summarise(biom = mean(biom)) 


h<- spread(h, FG, biom, fill=0)
colnames(h)[14:16]<-c('browser', 'grazer', 'scraper')
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
glm_gam_test(h.pred, exp.names = exp.names, 'grazerlog10', family='gaussian')
glm_gam_test(h.pred, exp.names = exp.names, 'scraperlog10', family='gaussian')
glm_gam_test(h.pred, exp.names = exp.names, 'browserlog10', family='gaussian')


mmi_tvalue(h.pred, exp.names = exp.names, 'grazerlog10', family='gaussian') ## hard coral strongest covariate
mmi_tvalue(h.pred, exp.names = exp.names, 'scraperlog10', family='gaussian') ##  hard coral is strongest covariate
mmi_tvalue(h.pred, exp.names = exp.names, 'browserlog10', family='gaussian') ## macroalgae is strongest covariate

options(na.action = "na.fail")
glm.biom<-glmer(scraper ~ hard.coral + macroalgae + rubble + substrate + complexity + 
  fish.biom + #Fished.Protected.dummy + Fished.Unfished.dummy + 
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
summary(glm.biom)
#dredge(glm.biom)

### repeat for functions
## scraper functions
load(file = 'results/models/scraper_function.Rdata')
h$management<-factor(h$management)

load(file = 'results/scraper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'scraping'))
summary(h.pred)

resid.glm<-glm(scraping  ~ biom, data = h.pred, family='Gamma'(link='log'))
h.pred$r<-resid(resid.glm)

glm<-glmer(scraping ~ hard.coral + macroalgae + rubble + substrate + complexity + 
        	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
          site.richness + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
glm2<-glmer(scraping ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
          site.richness + site.size + biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
summary(glm)

glm.r<-lmer(r ~ hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
          site.richness + site.size +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred)


visreg::visreg(glm)
summary(glm)
dredge(glm)
sjPlot::plot_models(glm, glm2)
rsquared(glm)

with(h, cor(site.richness, scale(biom)))
with(h, cor(log10(site.richness), scale(log10(biom))))

pairs2(dplyr::select_if(h.pred, is.numeric), 
  lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)


## grazer functions
load(file = 'results/models/cropper_function.Rdata')
h$management<-factor(h$management)

load(file = 'results/cropper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))


glm<-glmer(cropping.gram.ha ~ hard.coral + macroalgae + rubble + substrate + complexity + 
	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
   site.richness + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action= na.omit)
glm1<-glmer(cropping.gram.ha ~ hard.coral + macroalgae + rubble + substrate + complexity + 
  fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
   site.richness + site.size + biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action= na.omit)

visreg::visreg(glm)
summary(glm)
dredge(glm)
sjPlot::plot_models(glm, glm1)
rsquared(glm)


## test how global means influence function effects
load(file = 'results/models/cropper_function_subset.Rdata')
h$management<-factor(h$management)

load(file = 'results/cropper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))


glm.sub<-glmer(cropping.gram.ha ~ hard.coral + macroalgae + rubble + substrate + complexity + 
  fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
   site.richness + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action= na.omit)

visreg::visreg(glm)
summary(glm)
dredge(glm)
sjPlot::plot_models(glm, glm.sub)
rsquared(glm.sub)
