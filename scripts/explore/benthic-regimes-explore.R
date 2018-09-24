##### Benthic Regimes- Exploratory Analysis #####
# Use multivariate analysis to look at any benthic regimes across the entire dataset and within regions
# Created: September 24, 2018
 
setwd("~/Documents/git_repos/grazing-gradients")
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)
theme_set(theme_bw())

# load data
load("data/wio_herb_benthic_merged.Rdata")
ls()
head(pred)
dim(pred)
str(pred)
pred$unique.id
############################################################


############################################################
#### Prepare Data for Multivariate Analysis ####

# Habitat Types: hard.coral macroalgae rubble
# Site= unique.id? or also by transect? 
# Need to make dataframe: site by habitat type 

pred <-pred %>% select(unique.id, site.number, transect, dataset, 
              hard.coral, macroalgae, rubble)

head(pred) 

