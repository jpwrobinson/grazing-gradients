
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
source('scripts/functions/beta_func_legendre.R')
load('data/wio_gbr_herb_master.Rdata')

## estimate beta diversity, following lefcheck et al. 2019
# estimate mean biomass per site per FG
h <- herb %>% 
  ## sum biomass per FG in each transect
        group_by(dataset,  reef, site, transect, 
                 unique.id, FG, species) %>%
          summarise(abund = sum(abundance.500m2)) %>%
  ## mean FG biomass across transects at each site
          group_by(dataset, reef, site, unique.id, FG, species) %>%
          summarise(abund = mean(abund)) %>%
		### change to presence absence
		mutate(pa = ifelse(abund > 0, 1, 0)) #%>% 
		# mutate(abund = NULL)



## spread to community matrix and drop extra columns
graze.mat<-data.frame(spread(h[h$FG == 'Herbivore Grazer',], species, abund, fill=0))
rownames(graze.mat)<-graze.mat$unique.id
graze.mat <- graze.mat[, -c(1:5)]


scrape.mat<-data.frame(spread(h[h$FG == 'Herbivore Scraper',], species, abund, fill=0))
rownames(scrape.mat)<-scrape.mat$unique.id
scrape.mat <- scrape.mat[, -c(1:5)]


g_beta<-beta.div(graze.mat, method = 'jaccard', nperm=1, sqrt.D=FALSE)
hist(g_beta$LCBD)


par(mfrow=c(2,2))
hist(g_lcbd_rich, main = 'Grazer - richdiff')
hist(s_lcbd_rich, main = 'Scraper - richdiff')
hist(g_lcbd_repl, main = 'Grazer - replacement')
hist(s_lcbd_repl, main = 'Scraper - replacement')


decomp <- beta.div.comp(graze.mat)

decomp$part # 60/40 richenss vs replacement
# part : Beta diversity partitioning -- 
#        1. Total beta div. =  0.34
#        2. Total replacement diversity = 0.193 
#        3. Total richness/abundance difference diversity (or nestedness) = 0.15
#        4. Total replacement div./Total beta div. = 0.56 %
#        5. Total richness/abundance diff. div. (or nestedness)/Total beta div. 0.44%
g.diversity <-data.frame(site = rownames(graze.mat),
                                  beta_rich = LCBD.comp(decomp$rich)$LCBD,
                                  beta_repl = LCBD.comp(decomp$repl)$LCBD)


decomp <- beta.div.comp(scrape.mat)

decomp$part # 60/40 richenss vs replacement
# part : Beta diversity partitioning -- 
#        1. Total beta div. =  0.322
#        2. Total replacement diversity = 0.169
#        3. Total richness/abundance difference diversity (or nestedness) = 0.15
#        4. Total replacement div./Total beta div. = 0.52 %
#        5. Total richness/abundance diff. div. (or nestedness)/Total beta div. 0.48%

s.diversity <-data.frame(site = rownames(scrape.mat),
                                  beta_rich = LCBD.comp(decomp$rich)$LCBD,
                                  beta_repl = LCBD.comp(decomp$repl)$LCBD)

save(graze.mat, scrape.mat, g.diversity, s.diversity, file = 'results/graze_beta_estimates.Rdata')