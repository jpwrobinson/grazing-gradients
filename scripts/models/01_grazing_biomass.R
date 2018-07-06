
## Script to fit models to biomass patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr)

# data load
load("data/wio_herb_benthic_merged.Rdata")
pred$depth[pred$dataset=='Seychelles']<-10
pred$depth[pred$dataset=='GBR']<-10

# estimate mean biomass per site per FG
# drop 3m sites in Chagos with LUDACRIS/Luka Modric biomass
h <- pred %>% filter(depth != 3) %>%
  ## sum biomass per FG in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG,
                         hard.coral, macroalgae, complexity) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG,
                         hard.coral, macroalgae, complexity) %>%
          summarise(biom = mean(biom)) 


ggplot(h, aes(x = hard.coral, y = biom, col=FG)) + geom_point()
hist(h$biom[h$FG == 'Herbivore Scraper'])


h<- spread(h, FG, biom, fill=0)
colnames(h)[11:13]<-c('browser', 'grazer', 'scraper')
h<-as.data.frame(h)

cor(h$hard.coral, h$complexity)
cor(h$hard.coral, h$macroalgae)

source('scripts/functions/plot-cor-functions.R')
pairs2(data.frame(h$hard.coral, h$macroalgae, h$complexity, as.numeric(h$management)), 
       lower.panel=panel.smooth2, upper.panel = panel.cor)

##scale vars
h$hard.coral <- scale(h$hard.coral)
h$macroalgae <- scale(h$macroalgae)
h$complexity <- scale(h$complexity)

# testing model fit
m<-lmer(log10(grazer + 1) ~ hard.coral + macroalgae * complexity + management + ## fixed
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h)
summary(m)

## habitat models for each FG, with model diagnostics
m.grazer <- lmer(log10(grazer+1) ~ hard.coral + complexity + macroalgae + (1 | dataset) + (1 | date), data = h)
summary(m.grazer)
plot(resid(m.grazer, type='response'), log10(h$grazer+1))

m.scraper <- lmer(log10(scraper+1) ~ hard.coral + complexity + macroalgae + (1 | dataset) + (1 | date), data = h)
summary(m.scraper)
plot(resid(m.scraper, type='response'), log10(h$scraper+1))

m.browser <- lmer(log10(browser+1) ~ hard.coral + complexity + macroalgae + (1 | dataset) + (1 | date), data = h)
summary(m.browser)
plot(resid(m.browser, type='response'), log10(h$browser+1))

## NOW PLOT PREDICTIONS
nd.hc<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                  macroalgae = 0, complexity = 0)

nd.ma<-data.frame(hard.coral = 0,
                  macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30), 
                  complexity = 0)

nd.com<-data.frame(hard.coral = 0,
                   macroalgae = 0, 
                   complexity = seq(min(h$complexity), max(h$complexity), length.out=30))

## predict biomass holding macroalgae + complexity constant, removing random effects
p.grazer<-predict(m.grazer, newdata=nd, re.form=NA)
p.scraper<-predict(m.scraper, newdata=nd, re.form=NA)
p.browser<-predict(m.browser, newdata=nd, re.form=NA)

## PLOT, transform to org biomass
par(mfrow=c(1,1))
plot(nd$hard.coral, 10^p.grazer, type='l',col=1, axes=F, ylab='Biomass (kg/ha)', xlab='Hard coral cover (%)', ylim=c(0, 35))
lines(nd$hard.coral, 10^p.scraper, col=2)
lines(nd$hard.coral, 10^p.browser, col=3)

axis(1, at = seq(min(h$hard.coral), max(h$hard.coral), length.out=5), 
     labels= round(seq(min(pred$hard.coral), max(pred$hard.coral), length.out=5), 0))
axis(2)

par(xpd=T)
text(x=-0.5, y=35, label='a) Hard coral', font=2)
