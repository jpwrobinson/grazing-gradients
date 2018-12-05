
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

# benthic regimes 
load("data/pca_allregions_kmeans4_clusters.Rdata") # pca.kmeans

## site lat lons
sites<-read.csv('data/sites_with_benthic_clusters.csv')
sites$km.cluster<-as.character(sites$km.cluster)
# Cluster 1: high rubble
# Cluster 2: high macroalgae
# Cluster 3: high substrate
# Cluster 4: high coral 

# big map
world <- map_data("world2") 
world<-fortify(world)

## little maps
# islands<-sf::read_sf(dsn='data/ne_10m_land/ne_10m_land.shp')
# islands<-fortify(islands)


## chagos
chashp<-rgdal::readOGR('data/shapes/chagos/Chagos_v6.shp')
chagos<-sp::spTransform(chashp, CRS("+proj=longlat +datum=WGS84"))

## separating chashp by bathymetry
## L4 = subtidal reef flat, drowned pass, drowned rim, drowned patch, land on reef
chagos.drowned<-chashp[chashp@data$L4_ATTRIB %in% 
      c('subtidal reef flat', 'drowned pass', 'drowned rim', 'drowned patch'),]
chagos.land<-chashp[chashp@data$L4_ATTRIB %in% c('land on reef'),]
chagos.lagoon<-chashp[chashp@data$L4_ATTRIB %in% c('drowned lagoon'),]

## seychelles
# isl<-sf::st_read("data/shapes/sey/all islands.shp")
isl<-rgdal::readOGR("data/shapes/sey/all islands.shp")
isl<-fortify(isl)

## GBR
# isl<-sf::st_read("data/shapes/sey/all islands.shp")
gbrshp<-rgdal::readOGR("data/shapes/gbr/Great_Barrier_Reef_Features.shp")
gbrshp<-fortify(gbrshp)

## Maldives
malshp<-rgdal::readOGR("data/shapes/mal/MDV_DevInfo_Admin0B.shp")
malshp<-fortify(malshp)


bbox<-data.frame(island = c('SEY','GBR', 'MAL', 'CHA'),
					xmin = c(55.3, 146.3, 72.5, 71),
					xmax=c(55.9, 148, 74, 72.5),
					ymin=c(-4.85, -19, 0.25, -5),
					ymax=c(-4.25, -18.2, 0.46, -7))

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

pal <- wesanderson::wes_palette("Rushmore1", 4, type = "continuous")
cols.named<-c('1' = pal[1], '2' = pal[2],'3' = pal[3],'4' = pal[4])

## world
world.plot<-ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_quickmap(xlim = c(30, 170), ylim = c(-25, 20), expand = TRUE,
  clip = "on") + 
  labs(x = '', y = '') +
  geom_rect(data = bbox, 
  	aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax=ymax, group=island), fill='transparent',lwd=0.5, col='red') +
  theme(plot.margin=unit(c(0.1,0,0,0), "mm"))


## seychelles
sey.plot<-ggplot() + geom_polygon(data = isl, aes(x = long, y = lat, group=group), fill=alpha('grey', 0.6)) + 
	coord_quickmap(xlim = c(55.3, 55.9), ylim = c(-4.85, -4.25), expand = TRUE,
  	clip = "on") + 
  labs(x = '', y = '') +
  geom_point(data = sites, aes(x = X, y = Y, col=km.cluster), alpha=0.8, size=2) +
  theme(plot.margin=unit(c(0,0,0,0), "mm"),
  	legend.position = 'none',
  axis.title=element_blank(),
  axis.ticks=element_blank(),
  axis.text = element_text(size =6)) + 
  scale_color_manual(values = cols.named)

## Maldives
mal.plot<-ggplot() + geom_polygon(data = malshp, aes(x = long, y = lat, group=group), fill=alpha('grey', 0.6)) + 
	coord_quickmap(ylim = c(0.15, 1), xlim = c(72.75, 73.7), expand = TRUE,
  	clip = "on") + 
  labs(x = '', y = '') +
  geom_point(data = sites, aes(x = X, y = Y, col=km.cluster), alpha=0.8, size=2) +
  theme(plot.margin=unit(c(0,0,0,0), "mm"),
  	legend.position = 'none',
  axis.title=element_blank(),
  axis.ticks=element_blank(),
  axis.text = element_text(size =6)) + 
  scale_color_manual(values = cols.named)


  ## Chagos
chagos.plot<-ggplot() + 
            geom_polygon(data = chagos.land, aes(x = long, y = lat, group=group), fill='grey', col='grey') + 
            geom_polygon(data = chagos.drowned, aes(x = long, y = lat, group=group),fill='grey') + 
            geom_polygon(data = chagos.lagoon, aes(x = long, y = lat, group=group),fill='white') + 
            geom_point(data = sites, aes(x = X, y = Y, col=km.cluster), alpha=0.8, size=2) +
  coord_quickmap(ylim = c(-5.2, -7.5), xlim = c(71, 72.7), expand = TRUE,
    clip = "on") + 
  labs(x = '', y = '') + 
    theme(plot.margin=unit(c(0,0,0,0), "mm"),
  	legend.position = 'none',
  axis.title=element_blank(),
  axis.ticks=element_blank(),
  axis.text = element_text(size =6)) + 
  scale_color_manual(values = cols.named)


## GBR
gbr.plot<-ggplot() + geom_polygon(data = gbrshp, aes(x = long, y = lat, group=group), fill=alpha('grey', 0.6)) + 
	coord_quickmap(ylim = c(-19, -18.2), xlim = c(146.3, 148), expand = TRUE,
  	clip = "on") + 
  labs(x = '', y = '') +
  geom_point(data = sites, aes(x = X, y = Y, col=km.cluster), alpha=0.8, size=2) +
  theme(plot.margin=unit(c(0,0,0,0), "mm"),
  	legend.position = 'none',
  axis.title=element_blank(),
  axis.ticks=element_blank(),
  axis.text = element_text(size =6)) + 
  scale_color_manual(values = cols.named)

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
middle<-plot_grid(sey.plot,mal.plot,chagos.plot,gbr.plot, nrow=1)
bm<-plot_grid(middle, bottom, nrow=2, align='h', rel_heights = c(1, 0.7))

pdf(file='figures/Figure1.pdf', height = 7, width =12)
plot_grid(world.plot, bm, nrow=2, align='v', rel_heights=c(0.7, 1))
dev.off()