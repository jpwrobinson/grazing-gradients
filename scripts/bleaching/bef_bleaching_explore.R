
library(here)
library(tidyverse)
library(rethinking)
library(tidybayes)
setwd(here('grazing-gradients/'))
theme_set(theme_sleek())

## 2014-17 data
load(file = 'results/models/scraper_function.Rdata')
pred<-h %>% filter(date %in% c(2014, 2017) & dataset == 'Seychelles') %>% as.data.frame()

## create ran effect id
pred$site_yr<-with(pred, paste(date, site, sep = '_'))

## add regime predictor
load('data/raw-data/SEY_UVC_fish_clean_1994-2017.Rdata')
pred$regime<-fish$state[match(pred$site, fish$Location)]
pred$state<-with(pred, ifelse(regime == 'Shifted', 0, 1))


### 1998 data
load(file = 'results/bleaching/scraper_function_seychelles.Rdata')
pred2<-h %>% filter(date %in% c(1994, 2005)) %>% as.data.frame()

## create dummy numeric variables
pred2$bleaching<-ifelse(pred2$date == 2005, 1, 0) ### nobleaching = 0, bleaching = 1
## create ran effect id
pred2$site_yr<-with(pred2, paste(date, site, sep = '_'))
## add regime pred2ictor
pred2$regime<-fish$state[match(pred2$site, fish$Location)]
pred2$state<-with(pred2, ifelse(regime == 'Shifted', 0, 1))


## estimate richness vals
rich <- fish %>% filter(FG.fine == 'Herbivore Scraper') %>%
			group_by(Year, Location, count) %>% 
			summarise(richness = n_distinct(Species)) %>%
			group_by(Year, Location) %>%
			summarise(richness = mean(richness)) %>% 
			mutate(site_yr = paste(Year, Location, sep = '_'))

pred$richness<-rich$richness[match(pred$site_yr, rich$site_yr)]
pred2$richness<-rich$richness[match(pred2$site_yr, rich$site_yr)]

g2<-ggplot(pred, aes(richness, scraping, col=date)) + 
			geom_point() + 
			stat_smooth(method = 'lm', se=FALSE) +
			facet_wrap(~regime) +
			theme(legend.position = c(0.4, 0.8), legend.title=element_blank())


g1<-ggplot(pred2, aes(richness, scraping, col=date)) + 
			geom_point() + 
			stat_smooth(method = 'lm', se=FALSE) +
			facet_wrap(~regime) +
			theme(legend.position = c(0.4, 0.8), legend.title=element_blank())

pdf(file = 'figures/bleaching/sey_richness_scraping_bleaching.pdf', height=7, width = 12)
gridExtra::grid.arrange(g1, g2)
dev.off()