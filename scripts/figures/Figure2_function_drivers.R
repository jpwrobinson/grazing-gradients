
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



# ## get covariate effect sizes from full model
# load(file = 'results/models/function_m_croppers.Rdata')
# crop.est<-get_model_data(m.grazer)
# crop.est$model<-'Croppers'

# load(file = 'results/models/function_m_scrapers.Rdata')
# scrape.est<-get_model_data(m.scraper)
# scrape.est$model<-'Scrapers'

# est<-data.frame(rbind(crop.est, scrape.est))

## load t values
load(file = 'results/models/tvalues_croppers.Rdata'); crop<-mm.crop[[1]]
load(file = 'results/models/tvalues_scrapers.Rdata'); scrape<-mm.scrape[[1]]

est<-rbind(crop, scrape)

## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##
          ## ------------ NOW PLOTTING FIGURES -------------- ## 
## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##

## setup formatting information

linewidth = 2
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('Croppers' = pal[5], 'Scrapers' = pal[12])
theme_set(theme_sleek())
ylab<-rev(c('Hard coral', 'Available\nsubstrate', 'Rubble', 'Macroalgae', 'Structural\ncomplexity',
        'Fishable\nbiomass', 'No-take reef', 'Pristine reef', 'Mean length'))#,'Species richness'))

## reorder factor levels here - careful this is manual, check plot is ok
## for full model plot
# est$term<-factor(est$term, levels=levels(est$term)[rev(c(7,1,4,6,10,9,8,5,2,3))])
## for t value plot
est$Var<-factor(est$Var)
est$Var<-factor(est$Var, levels=levels(est$Var)[rev(c(5,9,7,6,3,4,1,2,8))])
est$indicator<-ifelse(est$indicator == 'cropping.gram.ha', 'Croppers', 'Scrapers')

## add var identifying strong and weak effects
#est$effect<-ifelse(est$p.value < 0.05, 'STRONG', 'WEAK')

## careful here for legend: colours defined by order of models, but other panels is defined by alphabetical order
g.effects <- ggplot(est, aes(Var, RI.t.abs, fill=indicator, col=indicator)) + 
              geom_hline(yintercept=0, linetype='dashed') +
              geom_point(size=2, position = position_dodge(width=0.4)) +
              # geom_pointrange(aes(ymin=RI.t.abs-var.t, ymax=RI.t.abs+var.t),size =0.75, position=position_dodge(width=0.4)) +
              scale_color_manual(values = cols.named) +
              scale_fill_manual(values = cols.named) +
             # scale_shape_manual(values = c(21,20)) + 
              guides(shape = FALSE) +
              labs(x='', y = 'Standardized effect size') +
              scale_y_continuous(breaks=seq(0, 1, 0.25), labels=c(0, 0.25, 0.5, 0.75, 1)) +
              scale_x_discrete(labels = ylab) +
              theme(legend.position = c(0.8, 0.8),
                legend.title=element_blank(),
                strip.text.x=element_text(size=14)) + coord_flip() +
              geom_vline(xintercept = 4.5, size=2, col='grey90') +
              annotate('text', x = 9.35, y = 6.6, label = 'A', fontface='bold', size=5) +
              annotate('text', x = 4.25, y = 6.6, label = 'B', fontface='bold', size=5)


ann_text <- data.frame(Var = factor('hard.coral'),RI.t.ratio = 0.99,lab = c("A",'B'),
                       indicator = factor(c('Croppers', 'Scrapers')))


g.rel.effects <- ggplot(est, aes(Var, RI.t.ratio, fill=indicator, col=indicator)) + 
              geom_hline(yintercept=0, linetype='dashed') +
              geom_bar(stat='identity', size=1, position = position_dodge(width=0.4)) +
              scale_color_manual(values = cols.named) +
              scale_fill_manual(values = cols.named) +
              facet_wrap(~ indicator) +
              guides(shape = FALSE) +
              labs(x='', y = 'Relative effect size') +
              scale_y_continuous(breaks=seq(0, 1, 0.25), labels=c(0, 0.25, 0.5, 0.75, 1)) +
              scale_x_discrete(labels = ylab) + coord_flip() +
              theme(legend.position = 'none',
                legend.title=element_blank(),
                axis.text=element_text(size=14),
                axis.title.x=element_text(size=18),
                strip.text.x=element_text(size=14)) + 
              geom_vline(xintercept = 4.5, size=2, col='grey90') #+
              # geom_text(data = ann_text,col='black', 
              #   aes(label = lab), size=6, vjust=-0.2, fontface='bold')



pdf(file = "figures/dump/Figure2_effect_sizes_tvalue.pdf", width=6, height=6)
g.effects
dev.off()


pdf(file = "figures/Figure2_rel_effect_sizes_tvalue.pdf", width=12, height=6)
g.rel.effects
dev.off()

# pdf(file = "figures/FigureS5_pred_biomass.pdf", width=12, height=8)
# plot_grid(top_side, ncol = 1, nrow=1)
# dev.off()


## ------------------------------------------------ ##

# END

