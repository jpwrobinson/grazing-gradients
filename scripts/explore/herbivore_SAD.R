## Species Abundance Distributions

setwd("~/Documents/git_repos/grazing-gradients")
load('data/wio_gbr_herb_master.Rdata')

library(tidyverse)
library(funk)
theme_set(theme_minimal())
## estimate mean species biomass per site
# estimate mean total biomass function per site
h <- herb %>% 
  ## sum biomass in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG, species) %>%
          summarise(biom=sum(biomass.kgha)) %>%
  ## mean biomass across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG, species) %>%
          summarise( biom=mean(biom)) %>%
  ## mean biomass per dataset, with SE
  			group_by(dataset, species, FG) %>%
  			summarise(se = se(biom), biom=mean(biom)) %>% 
  			mutate(se=ifelse(is.na(se), 0, se))


g1<-h %>% filter(FG == 'Herbivore Grazer') %>% ungroup() %>%
		mutate(species = fct_reorder(species, biom)) %>%
		ggplot() + 
		geom_pointrange(aes(species, biom, ymin = biom-se, ymax=biom+se, col=dataset)) +
		coord_flip() +
		labs(x = '', y = 'biomass (kg/ha)', title='Croppers') +
		theme(legend.position = c(0.8, 0.2), legend.title=element_blank())

g2<-h %>% filter(FG == 'Herbivore Scraper') %>% ungroup() %>%
		mutate(species = fct_reorder(species, biom)) %>%
		ggplot() + 
		geom_pointrange(aes(species, biom, ymin = biom-se, ymax=biom+se, col=dataset)) +
		coord_flip() +
		labs(x = '', y = 'biomass (kg/ha)', title='Scrapers') +
		theme(legend.position = 'none')

g3<-h %>% filter(FG == 'Herbivore Browser') %>% ungroup() %>%
		mutate(species = fct_reorder(species, biom)) %>%
		ggplot() + 
		geom_pointrange(aes(species, biom, ymin = biom-se, ymax=biom+se, col=dataset)) +
		coord_flip() +
		labs(x = '', y = 'biomass (kg/ha)', title='Browsers') +
		theme(legend.position = 'none')

library(gridExtra)

pdf(file='figures/explore/herbivore_SADs.pdf', height=8, width=15)

## raw biomass
grid.arrange(g1, g2, g3, nrow=1)

## log10 biomass
g1 <- g1 + scale_y_log10() 
g2 <- g2 + scale_y_log10() 
g3 <- g3 + scale_y_log10() 
grid.arrange(g1, g2, g3, nrow=1)

dev.off()