
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)

setwd(here('grazing-gradients'))


## get diversity
# data load
load("data/wio_herb_benthic_merged.Rdata")
# estimate mean biomass per site per FG
h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, species) %>%
          summarise(biom = mean(biom)) 

head(data.frame(h))
## change names for colnames
com.mat<-tidyr::spread(h, species, biom)
com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -1]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format
com.mat<-as.matrix(com.mat)
rownames(com.mat)<-unique(t$SZ.month)
dim(com.mat)


## estimate diversity
library(vegan)
div<-data.frame(div=diversity(com.mat), 
				richness=specnumber(com.mat), 
				unique.id = rows)
div$J <- div$div/log(div$richness)


## scraping models
load("results/models/scraping_model.Rdata")
rsquared(m.scraper)
# Marginal Conditional
#  0.1459465   0.6475017

## scraping data
load('results/models/scraper_function.Rdata')

h$resid<-resid(m.scraper)
## attach to t
h$simpson.diversity<-div$div[match(h$unique.id, div$unique_id)] ## simpson is 1 - D. 
h$sp.richness<-div$richness[match(h$unique.id, div$unique_id)]
h$evenness<-div$J[match(h$unique.id, div$unique_id)]

## assign seychelles 2017 with mean complexity values for now - needs fixed
h$complexity[h$dataset == 'Seychelles' & h$date == 2017] <- mean(h$complexity)

## plot expected relationships
pdf(file='figures/explore/scraping_diversity.pdf', height =5 ,width=9)
g1<-ggplot(h, aes( sp.richness, scraping, col=dataset))+ geom_point() + theme(legend.position='none')
g2<-ggplot(h, aes( evenness, scraping, col=dataset))+ geom_point() + 
theme(legend.position=c(0.6, 0.9), legend.title=element_blank())
gridExtra::grid.arrange(g1,g2, nrow=1)
dev.off()

## scale vars to keep covariate means = 0. This is helpful for comparing effect sizes when covariates are on different scales.
h$hard.coral <- scale(h$hard.coral)
h$macroalgae <- scale(h$macroalgae)
h$complexity <- scale(h$complexity)
h$rubble <- scale(h$rubble)
h$substrate <- scale(h$substrate)
h$fish.biom <- scale(h$fish.biom)

h$simpson.diversity <- scale(h$simpson.diversity)
h$sp.richness <- scale(h$sp.richness)
h$evenness <- scale(h$evenness)

## make dummy variables
h$fish.dummy<-ifelse(h$management=='Fished', 1, 0)
h$pristine.dummy<-ifelse(h$management=='Unfished', 1, 0)
# we use 2 dummy variables for 3 levels

m<-lmer(scraping ~ hard.coral + macroalgae + rubble + substrate + complexity + 
			fish.biom + fish.dummy + pristine.dummy + ## fixed 
			simpson.diversity + sp.richness +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h)
summary(m)
rsquared(m)

m<-lmer(resid ~ evenness + sp.richness +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h)
summary(m)
rsquared(m)


## refit without low diversity outliers
m.sub<-lmer(resid ~ evenness + sp.richness +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h[!h$evenness< -1,])
summary(m.sub)
## negative evenness effect is not due to low diversity outliers

par(mfrow=c(2,2))
visreg::visreg(m.sub)

