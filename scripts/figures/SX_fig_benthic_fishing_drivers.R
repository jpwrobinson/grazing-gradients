

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr); library(funk)
theme_set(theme_sleek())

load(file = 'results/models/tvalues_croppers_biom.Rdata'); mm[[1]]$response<-'biom'; crop.biom<-mm[[1]]
load(file = 'results/models/tvalues_croppers_site.rarefied.Rdata'); mm[[1]]$response<-'rare'; crop.rare<-mm[[1]]
load(file = 'results/models/tvalues_croppers_site.beta.Rdata'); mm[[1]]$response<-'beta'; crop.beta<-mm[[1]]

crop<-rbind(crop.biom, crop.rare, crop.beta)

ggplot(crop, aes(Var, RI.t.abs, fill = class)) + geom_bar(stat='identity') + 
			facet_wrap(~response) +
			coord_flip() +
			labs(x = '', y = 'Standardized effect size')

load(file = 'results/models/tvalues_scrapers_biom.Rdata'); mm[[1]]$response<-biom; scrap.biom<-mm[[1]]
load(file = 'results/models/tvalues_scrapers_site.rarefied.Rdata'); mm[[1]]$response<-rare; scrap.rare<-mm[[1]]
load(file = 'results/models/tvalues_scrapers_site.beta.Rdata'); mm[[1]]$response<-beta; scrap.beta<-mm[[1]]

scrap<-rbind(scrap.biom, scrap.rare, scrap.beta)

ggplot(scrap, aes(class, RI.t.abs, fill = Var)) + geom_bar(stat='identity') + 
			facet_wrap(~response) +
			coord_flip() +
			labs(x = '', y = 'Standardized effect size')

load(file = 'results/models/rsq_partial.Rdata')
rcrop$class<-ifelse(rcrop$Effect %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
rcrop <- rcrop %>% filter(Effect != 'Model')

rscrap$class<-ifelse(rscrap$Effect %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
rscrap <- rscrap %>% filter(Effect != 'Model')

g1<-ggplot(rcrop, aes(response, Rsq*100, fill = class)) + geom_bar(stat = 'identity') + 
		coord_flip() +
		labs(x = '', y = 'Partial R-squared')

g2<-ggplot(rscrap, aes(response, Rsq*100, fill = class)) + geom_bar(stat = 'identity') + 
		coord_flip() +
		labs(x = '', y = 'Partial R-squared')
