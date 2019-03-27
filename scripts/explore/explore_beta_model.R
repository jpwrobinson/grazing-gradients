library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)

setwd(here('grazing-gradients'))


load(file = 'results/models/decouple_biodiv_mods.Rdata')
load(file = 'results/models/decouple_biodiv_preds.Rdata')

beta.crop.res$dataset_real<-crop.pred$dataset

ggplot(beta.crop.res, aes(site.beta, visregRes, col=dataset_real)) + geom_point()