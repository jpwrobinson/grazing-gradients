
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
load('results/models/scraper_function_species.Rdata')
scrape <- h.sp

## add scraping proportion per site
scrape<-scrape %>% group_by(unique.id) %>% 
				mutate(total = sum(scraping)) %>%
				group_by(species, unique.id) %>%
				mutate(scraping.proportion = scraping / total*100) 


load(file = 'results/scraper_attributes.Rdata')
head(species.attributes)
## match in species level predictors/info
scrape$mean.species.biom<-species.attributes$biom[match(scrape$species, species.attributes$species)]
scrape$mean.richness<-species.attributes$mean.richness[match(scrape$species, species.attributes$species)]
scrape$mean.species.size.cm<-species.attributes$size.cm[match(scrape$species, species.attributes$species)]
scrape$mean.bite.rate<-species.attributes$bite.rate[match(scrape$species, species.attributes$species)]
scrape$rarity<-species.attributes$freq[match(scrape$species, species.attributes$species)]
## match in site level predictors
scrape$site.richness<-diversity.preds$richness[match(scrape$unique.id, diversity.preds$unique.id)]
scrape$site.size<-diversity.preds$mean.size[match(scrape$unique.id, diversity.preds$unique.id)]

scrape<-data.frame(scrape)

## add average species size per site
load("data/wio_herb_benthic_merged.Rdata")
sizes<-pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, species,
                 unique.id) %>%
          summarise(size = mean(length.cm), mass= mean(mass.g)) 
scrape$mean.species.site.size<-sizes$size

## scraping area increases as species richness increases
ggplot(scrape, aes(site.richness, scraping)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'area scraped', title='Species scraping area by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

## scraping contribution declines as species richness increases
ggplot(scrape, aes(site.richness, scraping.proportion)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm', se = FALSE) +
	labs(y = '% area scraped', title='Species scraping contribution by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())


scrape %>% group_by(unique.id, dataset, site.richness) %>% summarise(abund = sum(biom)) %>%
ggplot( aes(site.richness, log10(abund))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
	stat_smooth(method='lm', se = FALSE) +
	labs(y = 'abundance', title='Community abundance by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())


ggplot(scrape, aes(site.richness, mean.species.site.size)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'mean size (cm)', title='Species average size by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())


## Scraping obviously increases with abundance and biomass. Is richness effect because size changes?
ggplot(scrape, aes(scraping, log10(mean.species.site.size))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
		facet_wrap(~species, scales='free') +
	stat_smooth(method='lm') +
	labs(x = 'mean size (cm)', y = 'area scraped', title='Species average size by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

ggplot(scrape, aes(scraping, log10(biom))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
		facet_wrap(~species, scales='free') +
	stat_smooth(method='lm') +
	labs(x = 'log10 biomass', y = 'area scraped', title='Species average size by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

ggplot(scrape, aes(mean.species.site.size, log10(abund))) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + 
		facet_wrap(~species, scales='free') +
	stat_smooth(method='lm') +
	labs(x = 'mean size', y = 'log10 abundance', title='Species average size by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())



### Plot and save data for scraping contributions
pdf(file = 'figures/overyielding/scaping_contribution.pdf', height= 7, width=12)

## scraping contribution declines as species richness increases
ggplot(scrape, aes(site.richness, scraping.proportion)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm', se = FALSE) +
	labs(y = '% area scraped', title='Species scraping contribution by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

## loop models and pull slopes
species<-unique(scrape$species)
slopes<-numeric(); stars<-numeric()
for(i in 1:length(species)){

	df<-scrape[scrape$species == species[i],]
	if(uniques(df$dataset)>2){
	m<-glmer(scraping.proportion ~ scale(site.richness) + (1 | dataset), df, family = "Gamma"(link='log'))}
	else {
		m<-glm(scraping.proportion ~ scale(site.richness), df, family = "Gamma"(link='log'))}

	slope = exp(coef(summary(m))[2,1])
	# se = coef(summary(m))[2,2]
	star = coef(summary(m))[2,4]
	slopes<-rbind(slopes, slope)
	stars<-rbind(stars, star)
}

slopes<-data.frame(slope = slopes[,1], star = stars[,1], species = species)
slopes$sig<-ifelse(stars < 0.05, 'YES', 'NO')

ggplot(slopes, aes(species, slope, label = species, col=sig)) +
			 geom_text() +
			 geom_point() +
			 coord_flip() +
			 geom_hline(yintercept=1) +
			 labs(x = '', y = 'Model coefficient') +
			 theme(axis.text.y = element_blank()) +
			 annotate('text', x = 37, y = 1.2, vjust=1.5, size=4,fontface=2, label = 'Scraping contribution increases with richness') +
			 annotate('text', x = 37, y = 0.8 , vjust=1.5, size=4,fontface=2, label = 'Scraping contribution decreases with richness')

dev.off()



### Plot and save data for scraping contributions
pdf(file = 'figures/overyielding/scaping_area.pdf', height= 7, width=12)

## scraping contribution declines as species richness increases
ggplot(scrape, aes(site.richness, scraping)) + geom_point(size=2, alpha=0.5, aes(col=dataset)) + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm', se = FALSE) +
	labs(y = 'area scraped', title='Species scraping by assemblage richness') +
	theme(legend.position = c(0.9, 0.05), legend.title=element_blank())

## loop models and pull slopes
species<-unique(scrape$species)
slopes<-numeric(); stars<-numeric()
for(i in 1:length(species)){

	df<-scrape[scrape$species == species[i],]
	if(uniques(df$dataset)>2){
	m<-glmer(scraping ~ scale(site.richness) + (1 | dataset), df, family = "Gamma"(link='log'))}
	else {
		m<-glm(scraping ~ scale(site.richness), df, family = "Gamma"(link='log'))}

	slope = exp(coef(summary(m))[2,1])
	star = coef(summary(m))[2,4]
	slopes<-rbind(slopes, slope)
	stars<-rbind(stars, star)
}

slopes<-data.frame(slope = slopes[,1], star = stars[,1], species = species)
slopes$sig<-ifelse(stars < 0.05, 'YES', 'NO')
ggplot(slopes, aes(species, slope, label = species, col=sig)) +
			 geom_text() +
			 geom_point() +
			 coord_flip() +
			 geom_hline(yintercept=1) +
			 labs(x = '', y = 'Model coefficient') +
			 theme(axis.text.y = element_blank()) +
			 annotate('text', x = 37, y = 1.2, vjust=1.5, size=4,fontface=2, label = 'Scraping area increases with richness') +
			 annotate('text', x = 37, y = 0.8 , vjust=1.5, size=4,fontface=2, label = 'Scraping area decreases with richness')

dev.off()

## 3 species do increase scraping area with richness
# - S. tricolor (0.05 - 0.15, all regions)
# - S. viridifucatus (0.09 - 0.18 m2, Chagos mainly)
# - S. chameleon 
## unlikely that these drive relationships. To test:
load('results/models/scraper_function.Rdata')
test<-scrapers.uvc %>% filter(species != 'Scarus chameleon') %>%
			group_by(dataset, reef, unique.id, transect, transect.area) %>%
          summarise(scraping = sum(scraping), biom=sum(biomass.kgha), abund=sum(abundance.500m2)) %>%
  ## mean scraping across transects at each site
		group_by(dataset, reef, unique.id, transect.area) %>%
          summarise(scraping = mean(scraping), biom=mean(biom), abund = mean(abund)) %>%
          ungroup() %>%
          mutate(scraping=ifelse(transect.area==100, scraping/0.01, scraping)) %>%
		  mutate(scraping=ifelse(transect.area==250, scraping/0.025, scraping)) %>%
          mutate(scraping =ifelse(is.na(transect.area), scraping/0.01539, scraping)) %>% ## sum within unique.id to account for different transect areas in Maldives
		group_by(dataset, reef, unique.id, transect.area) %>%
		summarise(scraping = sum(scraping), biom = sum(biom), abund = sum(abund)) %>%		
			group_by(dataset, reef, unique.id) %>%
          summarise(scraping = sum(scraping), biom=sum(biom), abund = sum(abund)) 

str(scrapers.uvc)
load('results/models/scraper_richness_size_effects.Rdata')
test$sp.richness.scaled<-h$sp.richness.scaled[match(test$unique.id, h$unique.id)]
test$mean.size.scaled<-h$mean.size.scaled[match(test$unique.id, h$unique.id)]
test$evenness.scaled<-h$evenness.scaled[match(test$unique.id, h$unique.id)]

m<-lmer(scraping ~ scale(biom) + sp.richness.scaled + evenness.scaled + mean.size.scaled +
          (1 | dataset/reef), test)
summary(m) ## full richness estimate = 0.133, for excluding following species
# - S. tricolor  = 0.12344
# - S. viridifucatus = 0.13476
# - S. chameleon  = 0.12965