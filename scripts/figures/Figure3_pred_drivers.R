
library(here)
setwd(here('grazing-gradients'))

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
theme_set(theme_sleek())
## organize data to make 1st 2 panels for macroalgae and substrate#########

load(file = 'results/models/scraper_function.Rdata')

## get mean reef values for overlaying figure points
h.means.scrap<-h %>% group_by(dataset, reef) %>% 
  summarise(scraping=mean(scraping), hard.coral=mean(hard.coral),
            macroalgae=mean(macroalgae), rubble=mean(rubble), 
            substrate=mean(substrate), complexity=mean(complexity), fish.biom=mean(fish.biom)) 

scrape.raw<-h

load(file = 'results/models/cropper_function.Rdata')
## get mean reef values for overlaying figure points
h.means.crop<-h %>% group_by(dataset, reef) %>% 
  summarise(cropping.gram.ha=mean(cropping.gram.ha), hard.coral=mean(hard.coral),
            macroalgae=mean(macroalgae), rubble=mean(rubble), 
            substrate=mean(substrate), complexity=mean(complexity), fish.biom=mean(fish.biom)) 

crop.raw<-h

## load pred values
load(file = 'results/models/tvalues_croppers.Rdata')
load(file = 'results/models/tvalues_scrapers.Rdata')

crop.pred<-mm.crop[[3]]
crop.var<-mm.crop[[4]]

scrape.pred<-mm.scrape[[3]]
scrape.var<-mm.scrape[[4]]


## plor cropper benthic effects
plot.pred<-crop.pred %>% select(substrate, macroalgae) %>% gather(var, pred)

sub.seq<-with(crop.raw, seq(min(substrate), max(substrate), length.out=100))
ma.seq<-with(crop.raw, seq(min(macroalgae), max(macroalgae), length.out=100))

sub.var<-data.frame(mm.crop[[3]]['substrate'] - mm.crop[[4]]['substrate'], 
	mm.crop[[3]]['substrate'] + mm.crop[[4]]['substrate'], 'substrate')
colnames(sub.var)<-c('lwr', 'upr', 'var')

ma.var<-data.frame(mm.crop[[3]]['macroalgae'] - mm.crop[[4]]['macroalgae'], 
	mm.crop[[3]]['macroalgae'] + mm.crop[[4]]['macroalgae'], 'macroalgae')
colnames(sub.var)<-c('lwr', 'upr', 'var')

plot.pred$seq<-c(sub.seq, ma.seq)
plot.pred$lwr<-c(sub.var[,1], ma.var[,1])
plot.pred$upr<-c(sub.var[,2], ma.var[,2])

pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])

g1<-ggplot(plot.pred, aes(seq, pred, fill=var)) + 
		geom_line(lwd=1.2, aes(col=var, linetype=var)) +
		scale_color_manual(values = c(cols[1], cols[1])) +
		scale_fill_manual(values = c(cols[1], cols[1])) +
		geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.2) +
		labs(x = 'Cover (%)', y = expression(paste("algal consumption, g ha"^-1,"min"^-1))) +
		theme(legend.position = 'none', 
			legend.title = element_blank()) +
		annotate('text', x = 43, y = 1, label='Macroalgae') +
		annotate('text', x = 56, y = 2.8, label='Available substrate') 


## plot complexity effects
plot.pred<-crop.pred %>% select(complexity) %>% mutate(crop = complexity, complexity = NULL)
plot.pred$scrape<-scrape.pred$complexity
plot.pred <- gather(plot.pred, var, pred)

crop.seq<-with(crop.raw, seq(min(complexity), max(complexity), length.out=100))
scrape.seq<-with(scrape.raw, seq(min(complexity), max(complexity), length.out=100))

crop.var<-data.frame(mm.crop[[3]]['complexity'] - mm.crop[[4]]['complexity'], 
	mm.crop[[3]]['complexity'] + mm.crop[[4]]['complexity'], 'complexity')
colnames(crop.var)<-c('lwr', 'upr', 'var')

scrape.var<-data.frame(mm.scrape[[3]]['complexity'] - mm.scrape[[4]]['complexity'], 
	mm.scrape[[3]]['complexity'] + mm.scrape[[4]]['complexity'], 'complexity')
colnames(scrape.var)<-c('lwr', 'upr', 'var')

plot.pred$seq<-c(crop.seq, scrape.seq)
plot.pred$lwr<-c(crop.var[,1], scrape.var[,1])
plot.pred$upr<-c(crop.var[,2], scrape.var[,2])


pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('crop' = pal[5], 'scrape' = pal[12])

g2<-ggplot(plot.pred, aes(seq, pred, fill=var)) + 
		geom_line(lwd=1.2, aes(col=var)) +
		geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.2) +
		scale_color_manual(values = cols.named) +
		scale_fill_manual(values = cols.named) +
		labs(x = 'Habitat complexity') +
		theme(legend.position = 'none', 
			legend.title = element_blank(),
			strip.background = element_blank(),
			strip.placement='outside') + 
		facet_wrap(~ var, nrow = 2, scales='free_y',
			 strip.position = "left", 
			labeller = as_labeller(c(crop = expression(paste("algal consumption, g ha"^-1,"min"^-1)),
				 scrape = expression(paste('area grazed m'^2,'ha'^-1, 'min'^-1))) ) ) +
		ylab(NULL)

### plot fishing effects for scrapers

fished<-mm.scrape[[3]]['Fished.Protected.dummy'][1,]
fished.var<-mm.scrape[[4]]['Fished.Protected.dummy'][1,]

protected<-mm.scrape[[3]]['Fished.Protected.dummy'][100,]
protected.var<-mm.scrape[[4]]['Fished.Protected.dummy'][100,]

pristine<-mm.scrape[[3]]['Fished.Unfished.dummy'][100,]
pristine.var<-mm.scrape[[4]]['Fished.Unfished.dummy'][100,]

preds<-data.frame(pred = c(fished, protected, pristine), var = c(fished.var, protected.var, pristine.var))
preds$lwr<-with(preds, pred - var)
preds$upr<-with(preds, pred + var)
preds$cov<-factor(c('Fished', 'Protected', 'Pristine'))
preds$cov<-factor(preds$cov, levels = levels(preds$cov)[c(1,3,2)])

g3<-ggplot(preds, aes(cov, pred)) + geom_pointrange(aes(ymin= lwr, ymax = upr)) +
	labs(x = '', y = expression(paste('area grazed m'^2,'ha'^-1, 'min'^-1))) +
	theme(legend.position = 'none')


pdf(file = 'figures/Figure3_predicted_effects.pdf', height = 6, width = 12)
plot_grid(g1, g2, g3, nrow=1, labels=c('A', 'B', 'C'))
dev.off()