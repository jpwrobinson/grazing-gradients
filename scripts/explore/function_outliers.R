
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

load('results/models/scraper_richness_size_effects.Rdata')
out.ref<-h %>% group_by(dataset) %>% 
			mutate(ref = mean (scraping), 
					diff = scraping - ref) %>% ungroup()

sds<-out.ref %>% group_by(dataset) %>% summarise(sd = 2*sd(diff))

out.ref$sds<-sds$sd[match(out.ref$dataset, sds$dataset)]
out.ref$outlier<-ifelse(abs(out.ref$diff) > out.ref$sds, 'YES', 'NO')

outs<-out.ref$unique.id[out.ref$outlier=='YES']

diversity.preds$outlier<-ifelse(diversity.preds$unique.id %in% outs, 'YES', 'NO')
ggplot(diversity.preds, aes(mean.size, scraping, col=outlier))+ geom_point()
ggplot(diversity.preds, aes(mean.biom, scraping, col=outlier))+ geom_point()
ggplot(diversity.preds, aes(lfi, scraping, col=outlier))+ geom_point()
ggplot(diversity.preds, aes(richness, scraping, col=outlier, label=unique.id)) + geom_text()
ggplot(diversity.preds, aes(J, scraping, col=outlier, label=unique.id)) + geom_text()

load(file='results/scraper_community_matrix.Rdata')
out.sp<-com.mat[which(sites$unique.id %in% outs), ]

out.sp<- gather(data.frame(out.sp), species, biom)
out.sp$unique.id<-rep(sites[sites$unique.id %in% outs,]$unique.id, times = 32)

ggplot(out.sp, aes(species, log10(biom))) + 
			geom_bar(stat='identity') + facet_wrap(~unique.id) + coord_flip()




