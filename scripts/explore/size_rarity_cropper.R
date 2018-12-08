
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
pdf(file='figures/explore/cropping_rarity.pdf', height= 7, width=12)

# data load
load('results/models/cropper_function_species.Rdata')
cropper <- h.sp

cropper.prop<-cropper %>% group_by(unique.id) %>% 
				mutate(total = sum(cropping.gram.ha)) %>%
				group_by(species, unique.id) %>%
				mutate(prop = cropping.gram.ha / total) %>%
				group_by(species) %>%
				summarise(cropper.contribution = mean(prop))

cropper.sp<-cropper %>% group_by(species) %>% 
				summarise(cropping = mean(cropping.gram.ha))

load("data/wio_herb_benthic_merged.Rdata")
# estimate mean biomass per site per FG
h <- pred %>% filter(FG == 'Herbivore Grazer') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, species) %>%
          summarise(biom = mean(biom)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, biom)
sites<-com.mat[,1]
com.mat<-com.mat[, -c(1)]
com.mat[is.na(com.mat)]<-0
com.mat<-as.matrix(com.mat)

save(sites, com.mat, file='results/cropper_community_matrix.Rdata')

## estimate diversity
library(vegan)
div<-data.frame(div=diversity(com.mat), 
				richness=specnumber(com.mat), 
				unique.id = sites)
div$J <- div$div/log(div$richness)


# save mean sizes 
sizes<-pred %>% filter(FG == 'Herbivore Grazer') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, species,
                 unique.id) %>%
          summarise(size = mean(length.cm), mass= mean(mass.g))
  ## mean species sizes across transects at each site
 sizes.sp<- sizes %>%   group_by(species) %>%
          summarise(size = mean(size), mass=mean(mass)) 

## create dataframe of species-level metrics - size, cropping, frequency
com.mat.inc<-com.mat
com.mat.inc[com.mat.inc>0]<-1
## measure resident site richness for each species
sp <- colnames(com.mat)
rsr<-matrix(NA, nrow=length(sp), ncol=2)
for(i in 1:dim(com.mat)[2]){

	ind<-which(com.mat.inc[,i]==1)
	t<-as.matrix(com.mat.inc[ind,])
	rsr[i,1]<-mean(rowSums(t))
	rsr[i,2]<-sp[i]

}

rsr<-data.frame(rsr); colnames(rsr)<-c('mean.richness', 'species')
rsr$mean.richness<-as.numeric(as.character(rsr$mean.richness))

## create master species level dataframe
freq<-data.frame(freq=colSums(com.mat.inc), species = colnames(com.mat.inc))
freq$biom<-h$biom[match(freq$species, h$species)]
freq$mean.richness<-rsr$mean.richness[match(freq$species, rsr$species)]
freq$size.cm<-sizes.sp$size[match(freq$species, sizes.sp$species)]
freq$size.g<-sizes.sp$mass[match(freq$species, sizes.sp$species)]
freq$cropper.prop<-cropper.prop$cropper.contribution[match(freq$species, cropper.prop$species)]
freq$cropping<-cropper.sp$cropping[match(freq$species, cropper.sp$species)]

p<-read.csv(file = 'results/functions/grazer_bites_predicted.csv')
freq$genus<-str_split_fixed(freq$species, '\ ', 2)[,1]
freq$bite.rate<-p$median[match(freq$species, p$class)]
freq$bite.rate[is.na(freq$bite.rate)]<-p$median[match(freq$genus[is.na(freq$bite.rate)], p$class)]
freq$bite.rate[is.na(freq$bite.rate)]<-p$median[p$class == 'global.mean']
freq$genus<-NULL

## add things to div
div$mean.size<-sizes$size[match(div$unique.id, sizes$unique.id)]
div$mean.biom<-h$biom[match(div$unique.id, h$unique.id)]

## add site richness to h dataframe
cropper$richness<-div$richness[match(cropper$unique.id, div$unique.id)]
cropper$size<-sizes$size[match(cropper$unique.id, sizes$unique.id)]
ggplot(cropper, aes(richness, cropping.gram.ha, size=biom)) + geom_point() + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'area cropped', title='Species cropping area by assemblage richness')


ggplot(cropper, aes(richness, size)) + geom_point() + facet_wrap(~species, scales='free_y') +
	stat_smooth(method='lm') +
	labs(y = 'mean size cm', title='Species average size by assemblage richness')


ggplot(freq, aes(reorder(species,freq), freq)) + geom_bar(stat='identity') + coord_flip()
ggplot(freq, aes(size.g, freq, size=biom))  + scale_x_log10() + geom_text(aes(label=species))


## large species have greater average contribution to cropping 
ggplot(freq, aes(size.g, cropper.prop, size=freq))  +
		 scale_x_log10() + 
		 geom_text(aes(label=species)) + stat_smooth(method = 'lm') +
		 labs(y = 'mean proportion of cropping per site', title='Species contribution to cropping by average size')

ggplot(freq, aes(size.g, cropping, size=freq))  +
		 scale_x_log10() + 
		 geom_text(aes(label=species)) + stat_smooth(method = 'lm') +
		 labs(y = 'mean cropped area per site', title='Species cropping by average size')

## calculate LFI for each site
lfi <- pred %>% filter(FG == 'Herbivore Grazer') %>% 
	mutate(large = ifelse(length.cm > 15, 'large', 'small')) %>%
	 group_by(dataset, reef, site, transect, 
                 unique.id) %>%
          mutate(biom = sum(biomass.kgha)) %>%
          group_by(dataset, reef, site, transect, 
                 unique.id, large) %>%
          summarise(lfi = sum(biomass.kgha)/unique(biom)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, large) %>%
          summarise(lfi = mean(lfi))  %>% filter(large == 'large')

### compare with richness
div$lfi<-lfi$lfi[match(div$unique.id, lfi$unique.id)]
div$lfi[is.na(div$lfi)]<-0

ggplot(div, aes(richness, lfi)) + geom_point() + stat_smooth(method = 'lm') +
		 labs(y = 'biomass proportion of fish > 15cm', title='proportion large croppers by species richness')


load(file = 'results/models/cropper_richness_size_effects.Rdata')
div$cropping<-h$cropping.gram.ha[match(div$unique.id, h$unique.id	)]

ggplot(div, aes( lfi, cropping, size=richness)) + geom_point() + stat_smooth(method = 'lm') +
		 labs(y = 'cropped area',x ='biomass proportion fish > 15cm', title='area cropped by proportion large croppers')



ggplot(div, aes(richness, cropping, size=lfi)) + geom_point() + stat_smooth(method = 'lm') +
		 labs(y = 'cropped area', title='cropping by richness with LFI')


## where is s. prasiognathus?
# sprasio<-unique(pred$unique.id[pred$species == 'Scarus prasiognathus'])
# # div$sprasio<-ifelse(div$unique.id %in% sprasio, 'YES', 'NO')
# ggplot(div, aes(richness, cropping)) + geom_point(aes(col=sprasio)) + stat_smooth(method = 'lm') +
# 		 labs(y = 'cropped area', title='cropping by richness with LFI')

dev.off()


### species attributes
pdf(file='figures/explore/cropping_species_attributes.pdf', height=7, width=14)

inds<-c('freq', 'biom', 'mean.richness', 'cropping', 'cropper.prop', 'size.cm', 'bite.rate')
for(i in 1:7){
	
	d<-freq	
	d$species <- factor(d$species, levels=d$species[order(d[,inds[i]],decreasing=F)])

	freq.plot<-gather(d, attribute, value, -species) %>% mutate(value= round(value, 2)) %>%
				group_by(attribute) %>% mutate(value = rescale(value, to =c(0, 1)))

	freq.plot<-freq.plot[!freq.plot$attribute=='size.g',]
	g<-ggplot(freq.plot, aes(species, value, fill=value)) + 
				geom_bar(stat='identity') + 
				coord_flip() + facet_grid(~attribute, scales='free_x')  + 
				scale_fill_gradient(low = 'red', high = 'darkgreen')
	print(g)
}

scaled<-scaler(freq,ID = 'species', cats=FALSE)
pairs2(scaled, 
	lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)

dev.off()

species.attributes<-freq
diversity.preds<-div

save(species.attributes, diversity.preds, file = 'results/cropper_attributes.Rdata')

