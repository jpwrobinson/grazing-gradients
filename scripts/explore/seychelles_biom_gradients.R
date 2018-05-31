
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); library(ggsidekick); theme_set(theme_sleek())

load('data/SEY_UVC_fish_clean_1994-2017.Rdata')

## estimate biomass gradient across management status 
biom<-fish %>% filter(FG.coarse=='Herbivore') %>%
		group_by(Location, Year, count, FG.fine, Management,state) %>% 
		summarise(biom = sum(biomass.kgha)) %>%
		group_by(Location, Year, FG.fine, Management,state) %>%
		summarise(biom = mean (biom))

ggplot(biom, aes(Year, log10(biom+1), fill=Management)) + geom_boxplot() + facet_wrap(~FG.fine)

## Biomass moderately higher in protected sites, but not strong biomass gradient

biom %>% group_by(FG.fine, state, Management) %>% summarise(mean(biom))

## 100% higher browser biom in protected
## 23% higher excavator biom in protected
## 23% higher grazer biom in protected
## 32% higher scraper biom in protected

## Note differences between recovering and shifted


