
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)

setwd(here('grazing-gradients'))


load("results/models/cropper_function.Rdata")
crop<-data.frame(h)
crop$sp <- 'croppers'

load("results/models/scraper_function.Rdata")
scrape<-data.frame(h)
scrape$sp <- 'scrapers'


crop.bites<-read.csv('results/functions/grazer_bites_predicted.csv')
crop.bites$col <- NULL; crop.bites$X <- NULL

scrap.bites<-read.csv('results/functions/scraper_bites_predicted.csv')
scrap.bites$col <- NULL; scrap.bites$X <- NULL



save(crop, scrape, file='forgit/UVC_grazing_rates.Rdata')
save(crop.bites, scrap.bites, file='forgit/predicted_bite_rates.Rdata')