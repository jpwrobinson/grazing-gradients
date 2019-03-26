
library(tidyverse)
library(cowplot)
library(adespatial)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)

setwd(here('grazing-gradients'))
# source('scripts/functions/beta_func_legendre.R')
load("data/wio_herb_benthic_merged.Rdata")

## estimate beta diversity, following lefcheck et al. 2019
# estimate mean biomass per site per FG
h <- pred %>% 
  ## sum biomass per FG in each transect
        group_by(dataset,  reef, site, transect, 
                 unique.id, FG, species) %>%
          summarise(abund = sum(abundance.500m2)) %>%
  ## mean FG biomass across transects at each site
          group_by(dataset, reef, site, unique.id, FG, species) %>%
          summarise(abund = mean(abund)) #%>%
		### change to presence absence
		# mutate(abund = ifelse(abund > 0, 1, 0)) #%>% 




## spread to community matrix and drop extra columns
graze.mat<-data.frame(spread(h[h$FG == 'Herbivore Grazer',], species, abund, fill=0))
graze.sites<-graze.mat$unique.id
graze.mat <- graze.mat[, -c(1:5)]


scrape.mat<-data.frame(spread(h[h$FG == 'Herbivore Scraper',], species, abund, fill=0))
scrape.sites<-scrape.mat$unique.id
scrape.mat <- scrape.mat[, -c(1:5)]

decomp <- beta.div.comp(graze.mat,coef="BS", quant=TRUE)

decomp$part 
# part : Beta diversity partitioning -- 
#        1. Total beta div. =  0.36
#        2. Total replacement diversity = 0.23
#        3. Total richness/abundance difference diversity (or nestedness) = 0.14
#        4. Total replacement div./Total beta div. = 0.63 %
#        5. Total richness/abundance diff. div. (or nestedness)/Total beta div. 0.37%
g.diversity <-data.frame(site = graze.sites,
                                  beta_rich = LCBD.comp(decomp$rich)$LCBD,
                                  beta_repl = LCBD.comp(decomp$repl)$LCBD)


decomp <- beta.div.comp(scrape.mat,coef="BS", quant=TRUE)

decomp$part # 60/40 richenss vs replacement
# part : Beta diversity partitioning -- 
#        1. Total beta div. =  0.31
#        2. Total replacement diversity = 0.192
#        3. Total richness/abundance difference diversity (or nestedness) = 0.12
#        4. Total replacement div./Total beta div. = 0.61 %
#        5. Total richness/abundance diff. div. (or nestedness)/Total beta div. 0.39%

s.diversity <-data.frame(site = scrape.sites,
                                  beta_rich = LCBD.comp(decomp$rich)$LCBD,
                                  beta_repl = LCBD.comp(decomp$repl)$LCBD)

save(graze.mat, scrape.mat, g.diversity, s.diversity, file = 'results/graze_beta_estimates.Rdata')



### explore estimates
load(file = 'results/cropper_attributes.Rdata')
g.diversity$site.richness<-diversity.preds$richness[match(g.diversity$site, diversity.preds$unique.id)]
g.diversity$site.evenness<-diversity.preds$J[match(g.diversity$site, diversity.preds$unique.id)]

load(file = 'results/scraper_attributes.Rdata')
s.diversity$site.richness<-diversity.preds$richness[match(s.diversity$site, diversity.preds$unique.id)]
s.diversity$site.evenness<-diversity.preds$J[match(s.diversity$site, diversity.preds$unique.id)]

g.diversity$dataset<-h$dataset[match(g.diversity$site,h$unique.id)]
s.diversity$dataset<-h$dataset[match(s.diversity$site,h$unique.id)]

pdf(file='figures/explore/beta_explore.pdf', height = 7, width=12)

g1<-ggplot(g.diversity, aes(site.richness, beta_rich, col = dataset)) + 
		geom_point() + theme(legend.position=c(0.7, 0.8), legend.title=element_blank()) +
		labs(title = 'Grazers')
g2<-ggplot(s.diversity, aes(site.richness, beta_rich, col = dataset)) + 
		geom_point() + theme(legend.position='none') +
		labs(title = 'Scrapers')
g3<-ggplot(g.diversity, aes(site.richness, beta_repl, col = dataset)) + 
		geom_point() + theme(legend.position='none') +
		labs(title = 'Grazers')
g4<-ggplot(s.diversity, aes(site.richness, beta_repl, col = dataset)) + 
		geom_point() + theme(legend.position='none') +
		labs(title = 'Scrapers')
cowplot::plot_grid(g1,g2, g3, g4, nrow=2)

g3<-ggplot(g.diversity, aes(beta_rich, beta_repl, col = dataset)) + 
		geom_point() + theme(legend.position='none') +
		labs(title = 'Grazers')
g4<-ggplot(s.diversity, aes(beta_rich, beta_repl, col = dataset)) + 
		geom_point() + theme(legend.position='none') +
		labs(title = 'Scrapers')
cowplot::plot_grid(g3, g4, nrow=2)


dev.off()