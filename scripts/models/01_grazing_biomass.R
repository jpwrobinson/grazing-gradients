
## Script to fit models to biomass patterns for each FG across habitat gradients

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr)

# data load
load("data/wio_herb_benthic_merged.Rdata")
pred$depth[pred$dataset=='Seychelles']<-10

# estimate mean biomass per site per FG
# drop 3m sites in Chagos with LUDICROUS biomass
h <- pred %>% filter(depth != 3) %>%
  ## sum biomass per FG in each transect
        group_by(dataset, date, reef, site, management, transect, unique.id, depth, FG,
                         hard.coral, macroalgae, complexity) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG,
                         hard.coral, macroalgae, complexity) %>%
          summarise(biom = mean(biom)) 


