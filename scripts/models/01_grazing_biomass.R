
## Script to fit models to biomass patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr)
theme_set(theme_bw())

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


ggplot(h, aes(x = hard.coral, y = biom, col=FG)) + geom_point()
ggplot(h, aes(management, log10(biom+1), fill=FG)) + geom_boxplot()
hist(h$biom[h$FG == 'Herbivore Scraper'])


h<- spread(h, FG, biom, fill=0)
colnames(h)[11:13]<-c('browser', 'grazer', 'scraper')
h<-as.data.frame(h)

cor(h$hard.coral, h$complexity)
cor(h$hard.coral, h$macroalgae)

source('scripts/functions/plot-cor-functions.R')
pairs2(data.frame(h$hard.coral, h$macroalgae, h$complexity, h$management), 
       lower.panel=panel.smooth2, upper.panel = panel.cor)

##scale vars
h$hard.coral <- scale(h$hard.coral)
h$macroalgae <- scale(h$macroalgae)
h$complexity <- scale(h$complexity)

## convert biomass to log10
h$grazerlog10<-log10(h$grazer+1)
h$scraperlog10<-log10(h$scraper+1)
h$browserlog10<-log10(h$browser+1)

# testing model fit
m<-lmer(grazerlog10 ~ hard.coral + macroalgae * complexity + management + ## fixed
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h)
summary(m)

## habitat models for each FG, with model diagnostics
m.grazer <- lmer(grazerlog10 ~ hard.coral + complexity + macroalgae + management + (1 | dataset) + (1 | date), data = h)
summary(m.grazer)
plot(resid(m.grazer, type='response'), h$grazerlog10)
hist(resid(m.grazer))

m.scraper <- lmer(scraperlog10 ~ hard.coral + complexity + macroalgae + management + (1 | dataset) + (1 | date), data = h)
summary(m.scraper)
plot(resid(m.scraper, type='response'), h$scraperlog10)
hist(resid(m.scraper))

m.browser <- lmer(browserlog10 ~ hard.coral + complexity + macroalgae + management + (1 | dataset) + (1 | date), data = h)
summary(m.browser)
plot(resid(m.browser, type='response'), h$browserlog10)
hist(resid(m.browser))

## NOW PLOT PREDICTIONS
nd<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                  macroalgae = 0, complexity = 0, management = 'Unfished')

## predict biomass holding macroalgae + complexity constant, removing random effects
p.grazer<-predict(m.grazer, newdata=nd, re.form=NA)
p.scraper<-predict(m.scraper, newdata=nd, re.form=NA)
p.browser<-predict(m.browser, newdata=nd, re.form=NA)

## PLOT, transform to org biomass
par(mfrow=c(1,1))
plot(nd$hard.coral, p.grazer, type='l',col=1, axes=F, ylab='Log10 Biomass (kg/ha)', xlab='Hard coral cover (%)', ylim=c(1, 4))
lines(nd$hard.coral, p.scraper, col=2)
lines(nd$hard.coral, p.browser, col=3)


axis(1, at = seq(min(h$hard.coral), max(h$hard.coral), length.out=5), 
     labels= round(seq(min(pred$hard.coral), max(pred$hard.coral), length.out=5), 0))
axis(2)

par(xpd=T)
text(x=-0.5, y=1000, label='Hard coral effect', font=2)
legend('topright', legend=c('Grazer', 'Scraper', 'Browser'), col=c(1,2,3), lty=1)

