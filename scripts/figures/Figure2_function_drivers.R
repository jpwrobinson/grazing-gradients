
library(here)
setwd(here('grazing-gradients'))

## load models and predictions
load("results/models/function_m_scrapers.Rdata"); rsquared(m.scraper)
load("results/models/function_m_grazers.Rdata"); rsquared(m.grazer)


library(piecewiseSEM)
require(gridExtra)
library(grid)
library(lme4)
#library(sjPlot)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)

## organize data to make 1st 2 panels for macroalgae and substrate#########

load(file = 'results/models/scraper_function.Rdata')

## get mean reef values for overlaying figure points
h.means<-h %>% group_by(dataset, reef) %>% 
  summarise(scraping=mean(scraping), hard.coral=mean(hard.coral),
            macroalgae=mean(macroalgae), rubble=mean(rubble), 
            substrate=mean(substrate), complexity=mean(complexity), fish.biom=mean(fish.biom)) 

load(file = 'results/models/cropper_function.Rdata')
## get mean reef values for overlaying figure points
h.means<-h %>% group_by(dataset, reef) %>% 
  summarise(cropping.gram.ha=mean(cropping.gram.ha), hard.coral=mean(hard.coral),
            macroalgae=mean(macroalgae), rubble=mean(rubble), 
            substrate=mean(substrate), complexity=mean(complexity), fish.biom=mean(fish.biom)) 



## get covariate effect sizes 
load(file = 'results/models/function_m_croppers.Rdata')
crop.est<-get_model_data(m.grazer)
crop.est$model<-'Croppers'

load(file = 'results/models/function_m_scrapers.Rdata')
scrape.est<-get_model_data(m.scraper)
scrape.est$model<-'Scrapers'

est<-data.frame(rbind(crop.est, scrape.est))

## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##
          ## ------------ NOW PLOTTING FIGURES -------------- ## 
## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##

## setup formatting information

linewidth = 2
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('Croppers' = pal[5], 'Scrapers' = pal[12])
theme_set(theme_sleek())
ylab<-rev(c('Hard coral', 'Available\nsubstrate', 'Rubble', 'Macroalgae', 'Habitat\ncomplexity',
        'Fishable\nbiomass', 'Fished reef', 'Pristine reef', 'Mean size','Species richness'))

## reorder factor levels here - careful this is manual, check plot is ok
est$term<-factor(est$term, levels=levels(est$term)[rev(c(7,1,4,6,10,9,8,5,2,3))])

## add var identifying strong and weak effects
est$effect<-ifelse(est$p.value < 0.05, 'STRONG', 'WEAK')

## careful here for legend: colours defined by order of models, but other panels is defined by alphabetical order
g.effects <- ggplot(est, aes(term, estimate, fill=model, col=model)) + 
              geom_hline(yintercept=0, linetype='dashed') +
              geom_pointrange(aes(ymin=conf.low, ymax=conf.high,shape=effect),size =0.75, position=position_dodge(width=0.4)) +
              scale_color_manual(values = cols.named) +
              scale_fill_manual(values = cols.named) +
              scale_shape_manual(values = c(21,20)) + 
              guides(shape = FALSE) +
              labs(x='', y = 'Standardized effect size') +
              scale_x_discrete(labels = ylab) +
              theme(legend.position = c(0.8, 0.8),
                legend.title=element_blank()) + coord_flip() +
              geom_vline(xintercept = 5.5, size=2, col='grey90') +
              geom_vline(xintercept = 1.5, size=2, col='grey90')


pdf(file = "figures/Figure2_effect_sizes.pdf", width=6, height=6)
g.effects
dev.off()

# pdf(file = "figures/FigureS5_pred_biomass.pdf", width=12, height=8)
# plot_grid(top_side, ncol = 1, nrow=1)
# dev.off()


## ------------------------------------------------ ##

# END

