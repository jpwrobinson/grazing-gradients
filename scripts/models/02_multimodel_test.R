
## Script to fit models to function patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr); library(funk); library('rms')
theme_set(theme_sleek())
library(piecewiseSEM); library(sjPlot)

## scraper functions
load(file = 'results/models/scraper_function.Rdata')
h$management<-factor(h$management)

load(file = 'results/scraper_attributes.Rdata')
## match in site level predictors
h$site.richness<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]

rare<-read.csv(file = 'results/rarefied_richness_scrapers.csv')
h$site.rarefied<-rare$qD[match(h$unique.id, rare$site)]

h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'scraping'))
summary(h.pred)

resid.glm<-glm(scraping  ~ biom, data = h.pred, family='Gamma'(link='log'))
h.pred$r<-resid(resid.glm)

glm<-glmer(scraping ~ hard.coral + macroalgae + rubble + substrate + complexity + 
        	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
          site.rarefied + site.size + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
glm2<-glmer(scraping ~ #hard.coral + macroalgae + rubble + substrate + complexity + 
          fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
          site.rarefied + site.size + abund + biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
summary(glm2)
# MuMIn::dredge(glm2)

# glm.r<-lmer(r ~ hard.coral + macroalgae + rubble + substrate + complexity + 
#           fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy + 
#           site.richness + site.size +
#           (1 | dataset/reef) , ## random, nested = reefs within datasets
#                 data = h.pred)

glm.test<-glmer(scraping ~ biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
glm.test2<-glmer(scraping ~ biom + abund +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
glm.test3<-glmer(scraping ~ biom + site.rarefied + abund +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
rsquared(glm.test)
rsquared(glm.test2)
rsquared(glm.test3)
AIC(glm.test, glm.test2, glm.test3)


mm<-mmi_tvalue(h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 
          'site.rarefied', 'site.size' ), indicator = 'scraping', family = 'Gamma')

ggplot(mm[[1]], aes(Var, RI.t.abs, ymin = RI.t.abs - var.t, ymax = RI.t.abs + var.t)) + 
    geom_pointrange() + coord_flip()

gather(mm[[3]], var, effect) %>% filter(var != 'indicator') %>% mutate(effect = as.numeric(effect)) %>% 
      ggplot() + geom_line(aes(x=1:1000, y=effect, group=var)) + facet_wrap(~var, scales='free_x')


options(na.action = 'na.fail')
visreg::visreg(glm)

MuMIn::dredge(glm2)
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

mm<-mmi_tvalue(h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy', 
          'site.richness', 'site.size' ), indicator = 'cropping.gram.ha', family = 'Gamma')

ggplot(mm[[1]], aes(Var, RI.t.abs, ymin = RI.t.abs - var.t, ymax = RI.t.abs + var.t)) + 
    geom_pointrange() + coord_flip()


  gather(mm[[3]], var, effect) %>% filter(var != 'indicator') %>% mutate(effect = as.numeric(effect)) %>% 
      ggplot() + geom_line(aes(x=1:1000, y=effect, group=var)) + facet_wrap(~var, scales='free_x')
# visreg::visreg(glm)
summary(glm)
# dredge(glm)
sjPlot::plot_models(glm, glm1)
rsquared(glm)


glm.test<-glmer(cropping.gram.ha ~ biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
glm.test2<-glmer(cropping.gram.ha ~ biom + site.richness +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
glm.test3<-glmer(cropping.gram.ha ~ biom + site.richness + abund +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'))
rsquared(glm.test)
rsquared(glm.test3)
AIC(glm.test, glm.test2, glm.test3)

summary(glm.test3)

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

# visreg::visreg(glm)
summary(glm)
# dredge(glm)
sjPlot::plot_models(glm, glm.sub)
rsquared(glm.sub)
