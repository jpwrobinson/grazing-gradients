
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(maptools)

setwd(here('grazing-gradients'))
theme_set(theme_sleek())
#------------------------#
#------ data load ------#
#------------------------#
# biomass
load("data/wio_herb_benthic_merged.Rdata")

# big map
world <- map_data("world2") 
world<-fortify(world)

## little maps
# islands<-sf::read_sf(dsn='data/ne_10m_land/ne_10m_land.shp')
# islands<-fortify(islands)

## seychelles
isl<-sf::st_read("data/shapes/sey/all islands.shp")
isl<-rgdal::readOGR("data/shapes/sey/all islands.shp")
isl<-fortify(isl)

bbox<-data.frame(island = 'SEY', 
					xmin = c(55.25),
					xmax=c(56),
					ymin=c(-4.9),
					ymax=c(-4.2))

# estimate mean biomass per site per FG
biom <- pred %>% 
  ## sum biomass per FG in each transect
  group_by(dataset, date, reef, site, management, transect, 
           unique.id, depth, FG) %>%
  summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
  group_by(dataset, date, reef, site, management, unique.id, depth, FG) %>%
  summarise(biom = mean(biom)) %>%
  group_by(dataset, FG) %>%
  summarise(se = 2*se(biom), biom = mean(biom)) %>%
  mutate(FG = str_replace_all(FG, 'Herbivore ', ''))

#------------------------#
#------ create map figs ------#
#------------------------#

## world
world.plot<-ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_quickmap(xlim = c(40, 170), ylim = c(-25, 20), expand = TRUE,
  clip = "on") + 
  labs(x = '', y = '') +
  geom_rect(data = bbox, 
  	aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax=ymax, group=island), lwd=4, col='red')


## seychelles
ggplot() + geom_polygon(data = isl, aes(x = long, y = lat, group=group), fill=alpha('grey', 0.6)) + 
	coord_quickmap(xlim = c(55.25, 56), ylim = c(-4.9, -4.2), expand = TRUE,
  	clip = "on") + 
  labs(x = '', y = '')


#------------------------#
#------ create biomass bars ------#
#------------------------#
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('Grazer' = pal[5], 'Scraper' = pal[12], 'Browser' = pal[18])

sey.bar<-ggplot(biom[biom$dataset == 'Seychelles',], 
	aes(FG, biom, fill = FG)) + 
	geom_bar(stat = 'identity') +
	geom_errorbar(aes(ymin=biom-se, ymax=biom+se), width=0,
                 position=position_dodge(.9), col='grey50') +
	scale_fill_manual(values = cols.named) +
		labs(x = '', y = expression('kg ha'^-1), parse=T) + 
		theme(legend.title=element_blank(),
			axis.line.x=element_blank(),
			axis.ticks.x=element_blank(),
			legend.position = c(0.2, 0.8)) +
		scale_x_discrete(labels=NULL) +
		lims(y = c(0, max(biom$biom)+350))

mal.bar<-ggplot(biom[biom$dataset == 'Maldives',], 
	aes(FG, biom, fill = FG)) + 
	geom_bar(stat = 'identity') +
	geom_errorbar(aes(ymin=biom-se, ymax=biom+se), width=0,
                 position=position_dodge(.9), col='grey50') +
	scale_fill_manual(values = cols.named) +
		labs(x = '', y = '', parse=T) + 
		theme(legend.title=element_blank(),
			axis.line.x=element_blank(),
			axis.ticks.x=element_blank(),
			legend.position = 'none') +
		scale_x_discrete(labels=NULL) +
		lims(y = c(0, max(biom$biom)+350))

chag.bar<-ggplot(biom[biom$dataset == 'Chagos',], 
	aes(FG, biom, fill = FG)) + 
	geom_bar(stat = 'identity') +
	geom_errorbar(aes(ymin=biom-se, ymax=biom+se), width=0,
                 position=position_dodge(.9), col='grey50') +
	scale_fill_manual(values = cols.named) +
		labs(x = '', y = '', parse=T) + 
		theme(legend.title=element_blank(),
			axis.line.x=element_blank(),
			axis.ticks.x=element_blank(),
			legend.position = 'none') +
		scale_x_discrete(labels=NULL) +
		lims(y = c(0, max(biom$biom)+350))

gbr.bar<-ggplot(biom[biom$dataset == 'GBR',], 
	aes(FG, biom, fill = FG)) + 
	geom_bar(stat = 'identity') +
	geom_errorbar(aes(ymin=biom-se, ymax=biom+se), width=0,
                 position=position_dodge(.9), col='grey50') +
	scale_fill_manual(values = cols.named) +
		labs(x = '', y = '', parse=T) + 
		theme(legend.title=element_blank(),
			axis.line.x=element_blank(),
			axis.ticks.x=element_blank(),
			legend.position = 'none') +
		scale_x_discrete(labels=NULL) +
		lims(y = c(0, max(biom$biom)+350))


bottom<-plot_grid(sey.bar, mal.bar, chag.bar, gbr.bar, nrow=1)

plot_grid(world.plot, world.plot, bottom, nrow=3, align='v')#, rel.widths=c(1,1,1), labels='AUTO')

