
rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); theme_set(theme_sleek()); library(bayesplot); library(rethinking)

pdf(file='figures/models/bite-rate-bayes/mcmc_pairs_bite_models.pdf', height= 7, width =12)

## GRAZER MODELS
load('results/models/bites_grazers.Rdata')

post<-extract.samples(graze.m)

species<-data.frame(post)[,1:9]; colnames(species)<-as.character(unique(grazers$sp))
fam<-data.frame(post)[, 10:15]; colnames(fam)<-c(as.character(unique(grazers$Genus)))
dat<-data.frame(post)[,15:17]; colnames(dat)<-as.character(unique(grazers$dataset))
scal<-data.frame(post)[,18:22]; colnames(scal)<- c('intercept', 'scale', 'sigma.species', 'sigma.genus', 'sigma.dataset')

mcmc_intervals(species) + ggtitle('Grazers: species parameters')
mcmc_intervals(fam) + ggtitle('Grazers: genera parameters')
mcmc_intervals(dat) + ggtitle('Grazers: dataset parameters')

mcmc_intervals(scal) + ggtitle('Grazers: scale parameters')

## SCRAPER MODELS
load('results/models/bites_scrapers.Rdata')

post<-extract.samples(scrape.m)
species<-data.frame(post)[,1:27]; colnames(species)<-as.character(unique(scrapers$sp))
fam<-data.frame(post)[, 28:31]; colnames(fam)<-c(as.character(unique(scrapers$Genus)))
region<-data.frame(post)[, 32:34]; colnames(region)<-as.character(unique(scrapers$dataset))
scal<-data.frame(post)[,35:40];colnames(scal)<- c('intercept', 'Size', 'scale', 'sigma.species', 'sigma.genus', 'sigma.dataset')

mcmc_intervals(species) + ggtitle('Scrapers: species parameters')
mcmc_intervals(fam) + ggtitle('Scrapers: genera parameters')
mcmc_intervals(region) + ggtitle('Scrapers: dataset parameters')
mcmc_intervals(scal) + ggtitle('Scrapers: scale + size parameters')


## BROWSER MODELS
load('results/models/bites_browsers.Rdata')

post<-extract.samples(browse.m)
fam<-data.frame(post)[,1:4]; colnames(fam)<-c(as.character(unique(browsers$Genus)), as.character(unique(browsers$dataset)))
scal<-data.frame(post)[,5:8]; colnames(scal)<-c('intercept', 'sigma', 'sigma.genus', 'sigma.dataset')

mcmc_intervals(fam) + ggtitle('Browsers: family and region parameters')
mcmc_intervals(scal) + ggtitle('Browsers: scale parameters')


dev.off()