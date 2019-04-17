library(funk)
library(scales)
library(here)
library(visreg)
library(lme4)
library(tidyverse)
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


load(file= 'results/models/scraper_function_resid.Rdata')
scrape$resid<-scrapers$resid[match(scrape$unique.id, scrapers$unique.id)]
scrape$resid.val<-ifelse(scrape$resid > 0, 'Overyield', 'Underyield')


ggplot(scrape, aes(mean.species.size.cm, scraping.proportion, col=resid.val)) + geom_point() + stat_smooth(method = 'lm')
ggplot(scrape, aes(mean.species.size.cm, log10(scraping), col=resid.val)) + geom_point() + stat_smooth(method = 'lm')


ggplot(scrape, aes(mean.species.biom, scraping, col=resid.val)) + 
			geom_point() + stat_smooth(method = 'lm') +
			scale_x_log10() +
			scale_y_log10()

m<-(lmer(log10(scraping) ~ scale(mean.species.biom)*resid.val + (1 | species), scrape))
visreg::visreg(m, 'mean.species.biom', by = 'resid.val')

# # data load
# load('results/models/cropper_function_species.Rdata')
# crop <- h.sp

# ## add scraping proportion per site
# crop<-crop %>% group_by(unique.id) %>% 
# 				mutate(total = sum(scraping)) %>%
# 				group_by(species, unique.id) %>%
# 				mutate(scraping.proportion = scraping / total*100) 


# load(file = 'results/cropper_attributes.Rdata')
# head(species.attributes)
# ## match in species level predictors/info
# crop$mean.species.biom<-species.attributes$biom[match(crop$species, species.attributes$species)]
# crop$mean.richness<-species.attributes$mean.richness[match(crop$species, species.attributes$species)]
# crop$mean.species.size.cm<-species.attributes$size.cm[match(crop$species, species.attributes$species)]
# crop$mean.bite.rate<-species.attributes$bite.rate[match(crop$species, species.attributes$species)]
# crop$rarity<-species.attributes$freq[match(crop$species, species.attributes$species)]
# ## match in site level predictors
# crop$site.richness<-diversity.preds$richness[match(crop$unique.id, diversity.preds$unique.id)]
# crop$site.size<-diversity.preds$mean.size[match(crop$unique.id, diversity.preds$unique.id)]

# crop<-data.frame(crop)


# load(file= 'results/models/cropper_function_resid.Rdata')
# crop$resid<-grazers$resid[match(crop$unique.id, grazers$unique.id)]
# crop$resid.val<-ifelse(crop$resid > 0, 'Overyield', 'Underyield')


# ggplot(crop, aes(mean.species.size.cm, log10(cropping.gram.ha), col=resid.val)) + geom_point() + stat_smooth(method = 'lm')


# ggplot(crop, aes(mean.species.biom, cropping.gram.ha, col=resid.val)) + 
# 			geom_point() + stat_smooth(method = 'lm') +
# 			scale_x_log10() +
# 			scale_y_log10()

# m<-(lmer(log10(cropping.gram.ha) ~ scale(mean.species.biom)*resid.val + (1 | species), crop))
# visreg::visreg(m, 'mean.species.biom', by = 'resid.val')