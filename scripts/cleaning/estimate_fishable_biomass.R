
setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(dplyr); library(tidyr); library(funk)
theme_set(theme_sleek())
load(file='data/wio_gbr_fish_master.Rdata'); fish<-herb


### estimate total fishable biomass at each site
### defined as the total biomass of all observed fish on ONE TRANSECT, averaged ACROSS TRANSECTS
biom <- fish %>% group_by(unique.id, transect) %>%
					summarise(biom = sum(biomass.kgha)) %>%
					ungroup() %>%
					group_by(unique.id) %>%
					summarise(biom = mean(biom))

save(biom, file = 'data/wio_gbr_fishable_biom.Rdata')