
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)
theme_set(theme_sleek())
setwd(here('grazing-gradients'))

# data load
load('results/models/cropper_function_species.Rdata')
cropper <- h.sp

## add cropping proportion per site
cropper<-cropper %>% group_by(unique.id) %>% 
				mutate(total = sum(cropping.gram.ha)) %>%
				group_by(species, unique.id) %>%
				mutate(cropping.proportion = cropping.gram.ha / total*100) 


load(file = 'results/cropper_attributes.Rdata')
head(species.attributes)
## match in species level predictors/info
cropper$mean.species.biom<-species.attributes$biom[match(cropper$species, species.attributes$species)]
cropper$mean.richness<-species.attributes$mean.richness[match(cropper$species, species.attributes$species)]
cropper$mean.species.size.cm<-species.attributes$size.cm[match(cropper$species, species.attributes$species)]
cropper$mean.bite.rate<-species.attributes$bite.rate[match(cropper$species, species.attributes$species)]
cropper$rarity<-species.attributes$freq[match(cropper$species, species.attributes$species)]
## match in site level predictors
cropper$site.richness<-diversity.preds$richness[match(cropper$unique.id, diversity.preds$unique.id)]
cropper$site.size<-diversity.preds$mean.size[match(cropper$unique.id, diversity.preds$unique.id)]

cropper<-data.frame(cropper)

## add average species size per site
load("data/wio_herb_benthic_merged.Rdata")
sizes<-pred %>% filter(FG == 'Herbivore Grazer') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, species,
                 unique.id) %>%
          summarise(size = mean(length.cm), mass= mean(mass.g)) 
cropper$mean.species.site.size<-sizes$size

## cropping area increases as species richness increases
ggplot(cropper, aes(site.richness, cropping.gram.ha)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'area cropped', title='Species cropping area by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

## cropping contribution declines as species richness increases
ggplot(cropper, aes(site.richness, cropping.proportion)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm', se = FALSE) +
	labs(y = '% area cropped', title='Species cropping contribution by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())


cropper %>% group_by(unique.id, dataset, site.richness) %>% summarise(abund = sum(biom)) %>%
ggplot( aes(site.richness, log10(abund))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
	stat_smooth(method='lm', se = FALSE) +
	labs(y = 'abundance', title='Community biomass by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())


ggplot(cropper, aes(site.richness, mean.species.site.size)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'mean size (cm)', title='Species average size by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())


## Scraping obviously increases with abundance and biomass. Is richness effect because size changes?
ggplot(cropper, aes(cropping.gram.ha, log10(mean.species.site.size))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
		facet_wrap(~species, scales='free') +
	stat_smooth(method='lm') +
	labs(x = 'mean size (cm)', y = 'area cropped', title='Species average size by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

ggplot(cropper, aes(cropping.gram.ha, log10(biom))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
		facet_wrap(~species, scales='free') +
	stat_smooth(method='lm') +
	labs(x = 'log10 biomass', y = 'area cropped', title='Species cropped area by assemblage biomass') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

ggplot(cropper, aes(mean.species.site.size, log10(abund))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
		facet_wrap(~species, scales='free') +
	stat_smooth(method='lm') +
	labs(x = 'mean size', y = 'log10 abundance', title='Species average size by assemblage abundance') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())



### Plot and save data for cropping contributions
pdf(file = 'figures/overyielding/cropping_contribution.pdf', height= 7, width=12)

## cropping contribution declines as species richness increases
ggplot(cropper, aes(site.richness, cropping.proportion)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm', se = FALSE) +
	labs(y = '% area cropped', title='Species cropping contribution by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

## loop models and pull slopes
species<-unique(cropper$species)
slopes<-numeric(); stars<-numeric(); speciess<-numeric()
for(i in 1:length(species)){

	df<-cropper[cropper$species == species[i],]
	if(dim(df)[1]>5){
	if(uniques(df$dataset)>2){
	m<-glmer(cropping.proportion ~ scale(site.richness) + (1 | dataset), df, family = "Gamma"(link='log'))}
	else {
		m<-glm(cropping.proportion ~ scale(site.richness), df, family = "Gamma"(link='log'))}

	slope = exp(coef(summary(m))[2,1])
	# se = coef(summary(m))[2,2]
	star = coef(summary(m))[2,4]
	slopes<-rbind(slopes, slope)
	stars<-rbind(stars, star)
	speciess<-rbind(speciess, species[i])
}}

slopes2<-data.frame(slope = slopes[,1], star = stars[,1], species = speciess[,1])
slopes2$sig<-ifelse(stars < 0.05, 'YES', 'NO')

ggplot(slopes2, aes(species, slope, label = species, col=sig)) +
			 geom_text() +
			 geom_point() +
			 coord_flip() +
			 geom_hline(yintercept=1) +
			 labs(x = '', y = 'Model coefficient') +
			 theme(axis.text.y = element_blank()) +
			 annotate('text', x = 37, y = 1.2, vjust=1.5, size=4,fontface=2, label = 'Scraping contribution increases with richness') +
			 annotate('text', x = 37, y = 0.8 , vjust=1.5, size=4,fontface=2, label = 'Scraping contribution decreases with richness')

dev.off()



### Plot and save data for cropping contributions
pdf(file = 'figures/overyielding/cropped_area.pdf', height= 7, width=12)

## cropping contribution declines as species richness increases
ggplot(cropper, aes(site.richness, cropping.gram.ha)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm', se = FALSE) +
	labs(y = 'area cropped', title='Species cropping by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

## loop models and pull slopes
species<-unique(cropper$species)
slopes<-numeric(); stars<-numeric()
for(i in 1:length(species)){

	df<-cropper[cropper$species == species[i],]
	if(dim(df)[1]>5){
	if(uniques(df$dataset)>2){
	m<-glmer(cropping.gram.ha ~ scale(site.richness) + (1 | dataset), df, family = "Gamma"(link='log'))}
	else {
		m<-glm(cropping.gram.ha ~ scale(site.richness), df, family = "Gamma"(link='log'))}

	slope = exp(coef(summary(m))[2,1])
	star = coef(summary(m))[2,4]
	slopes<-rbind(slopes, slope)
	stars<-rbind(stars, star)
	speciess<-rbind(speciess, species[i])
}}

slopes2<-data.frame(slope = slopes[,1], star = stars[,1], species = speciess[,1])
slopes2$sig<-ifelse(slopes2$star < 0.05, 'YES', 'NO')

ggplot(slopes2, aes(species, slope, label = species, col=sig)) +
			 geom_text() +
			 geom_point() +
			 coord_flip() +
			 geom_hline(yintercept=1) +
			 labs(x = '', y = 'Model coefficient') +
			 theme(axis.text.y = element_blank()) +
			 annotate('text', x = 37, y = 1.2, vjust=1.5, size=4,fontface=2, label = 'Scraping area increases with richness') +
			 annotate('text', x = 37, y = 0.8 , vjust=1.5, size=4,fontface=2, label = 'Scraping area decreases with richness')

dev.off()
