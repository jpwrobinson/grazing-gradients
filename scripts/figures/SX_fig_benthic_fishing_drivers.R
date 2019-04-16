

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(ggplot2); library(visreg); library(lme4); library(dplyr); library(tidyr); library(funk)
theme_set(theme_sleek())

load(file = 'results/models/tvalues_croppers_biom.Rdata'); mm$response<-'biom'; crop.biom<-mm
load(file = 'results/models/tvalues_croppers_site.rarefied.Rdata'); mm$response<-'rare'; crop.rare<-mm
load(file = 'results/models/tvalues_croppers_site.beta.Rdata'); mm$response<-'beta'; crop.beta<-mm


crop<-rbind(crop.biom, crop.rare, crop.beta)
crop$response_fac<-factor(crop$response, levels=c('biom', 'rare', 'beta'))
crop$Var<-factor(crop$Var)
crop<-crop %>% group_by(class) %>% mutate(Var = forcats::fct_reorder(.f=Var, .x=class))

ggplot(crop, aes(Var, RI.t.ratio, fill = class)) + geom_bar(stat='identity') + 
			facet_wrap(~response_fac) +
			coord_flip() +
			labs(x = '', y = 'Sum of standardized T-values') +
		# scale_x_discrete(labels = c('Biomass', 'Richness', 'Beta')) +
		scale_fill_manual(values = c('#d95f02', '#1b9e77')) +
		# scale_y_continuous(breaks = seq(0, 1, 0.2)) +
		scale_color_manual(values = rep('white', 9)) +
		theme(legend.position = 'bottom', legend.title=element_blank())


load(file = 'results/models/tvalues_scrapers_biom.Rdata'); mm$response<-'biom'; scrap.biom<-mm
load(file = 'results/models/tvalues_scrapers_site.rarefied.Rdata'); mm$response<-'rare'; scrap.rare<-mm
load(file = 'results/models/tvalues_scrapers_site.beta.Rdata'); mm$response<-'beta'; scrap.beta<-mm


scrap<-rbind(scrap.biom, scrap.rare, scrap.beta)
scrap$response_fac<-factor(scrap$response, levels=c('biom', 'rare', 'beta'))
scrap$Var<-factor(scrap$Var)
scrap<-scrap %>% group_by(class) %>% mutate(Var = forcats::fct_reorder(.f=Var, .x=class))

ggplot(scrap, aes(Var, RI.t.ratio, fill = class)) + geom_bar(stat='identity') + 
			facet_wrap(~response_fac) +
			coord_flip() +
			labs(x = '', y = 'Sum of standardized T-values') +
		# scale_x_discrete(labels = c('Biomass', 'Richness', 'Beta')) +
		scale_fill_manual(values = c('#d95f02', '#1b9e77')) +
		# scale_y_continuous(breaks = seq(0, 1, 0.2)) +
		scale_color_manual(values = rep('white', 9)) +
		theme(legend.position = 'bottom', legend.title=element_blank())


scrap %>% group_by(class, response) %>% summarise(eff = mean(RI.t.ratio))

ggplot(h.pred, aes(site.size, biom)) + geom_point()














load(file = 'results/models/rsq_partial.Rdata')
rcrop$class<-ifelse(rcrop$Effect %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
rcrop <- rcrop %>% filter(Effect != 'Model')

rscrap$class<-ifelse(rscrap$Effect %in% c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity'), 'benthic', 'fishing')
rscrap <- rscrap %>% filter(Effect != 'Model')
rscrap$response_fac<-factor(rscrap$response, levels=c('biom', 'site.rarefied', 'site.beta'))


levels(rscrap$Effect)
rscrap<-rscrap %>% group_by(class) %>% mutate(Effect = forcats::fct_reorder2(.f=Effect, .x=Rsq, .y=class))
levels(rscrap$Effect)

g1<-ggplot(rcrop, aes(response, Rsq*100, fill = class)) + geom_bar(stat = 'identity') + 
		coord_flip() +
		labs(x = '', y = 'Partial R-squared')

ggplot(rscrap, aes(Effect, Rsq*100)) + 
		geom_bar(aes(fill = class), stat = 'identity') + 
		# geom_bar(aes(col = Effect), fill='transparent', stat = 'identity') + 
		coord_flip() +
		facet_wrap(~ response) +
		labs(x = '', y = 'Partial R-squared') +
		# scale_x_discrete(labels = c('Biomass', 'Richness', 'Beta')) +
		scale_fill_manual(values = c('#d95f02', '#1b9e77')) +
		scale_y_continuous(breaks = seq(0, 30, 5), labels=c('0%','5%', '10%','15%', '20%','25%', '30%')) +
		scale_color_manual(values = rep('white', 9))


pdf(file = 'figures/fig_rsq_benthic_fishing.pdf', height= 7, width =9)
rscrap %>% group_by(class, response) %>% summarise(eff = sum(Rsq)*100) %>%
	ggplot() + geom_bar(aes(response, eff, fill=class), stat='identity', position=position_dodge()) +
	scale_fill_manual(values = c('#d95f02', '#1b9e77')) +
		scale_y_continuous(breaks = seq(0, 30, 5), labels=c('0%','5%', '10%','15%', '20%','25%', '30%'))  +
		labs(x = '', y = 'Partial R-squared')  +
		scale_x_discrete(labels = c('Biomass', 'Richness', 'Beta')) +
		theme(axis.text=element_text(size=14),
                axis.title=element_text(size=14), 
                legend.position = c(0.8, 0.8), 
                legend.title=element_blank(), 
                legend.key.width = unit(1,"cm"),
                legend.text = element_text(size =14))
dev.off()


