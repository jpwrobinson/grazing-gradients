
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

th<-theme(axis.text=element_text(size=14),
                axis.title=element_text(size=14))
## organize data to make 1st 2 panels for macroalgae and substrate#########

load(file = 'results/models/scraper_function.Rdata')

## get mean reef values for overlaying figure points
h.means.scrap<-h %>% group_by(dataset, reef) %>% 
  summarise(scraping=mean(scraping), hard.coral=mean(hard.coral),
            macroalgae=mean(macroalgae), rubble=mean(rubble), 
            substrate=mean(substrate), complexity=mean(complexity), fish.biom=mean(fish.biom)) 

scrape.raw<-h

load(file = 'results/models/cropper_function.Rdata')
load(file = 'results/cropper_attributes.Rdata')
h$site.size<-diversity.preds$mean.size[match(h$unique.id, diversity.preds$unique.id)]
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
# size.seq<-with(crop.raw, seq(min(site.size), max(site.size), length.out=100))

sub.var<-data.frame(mm.crop[[3]]['substrate'] - mm.crop[[4]]['substrate'], 
	mm.crop[[3]]['substrate'] + mm.crop[[4]]['substrate'], 'substrate')

ma.var<-data.frame(mm.crop[[3]]['macroalgae'] - mm.crop[[4]]['macroalgae'], 
	mm.crop[[3]]['macroalgae'] + mm.crop[[4]]['macroalgae'], 'macroalgae')

# size.var<-data.frame(mm.crop[[3]]['site.size'] - mm.crop[[4]]['site.size'], 
# 	mm.crop[[3]]['site.size'] + mm.crop[[4]]['site.size'], 'site.size')

plot.pred$seq<-c(sub.seq, ma.seq)
plot.pred$lwr<-c(sub.var[,1], ma.var[,1])
plot.pred$upr<-c(sub.var[,2], ma.var[,2])

pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])

## deciles for plot rugs
deciles<-data.frame(
  macroalgae = quantile(h$macroalgae, prob=seq(0, 1, length.out=11)),
  substrate = quantile(h$substrate, prob=seq(0, 1, length.out=11)),
  complexity = quantile(h$complexity, prob=seq(0, 1, length.out=11)),
  site.size = quantile(h$site.size, prob=seq(0, 1, length.out=11)))

# g1<-ggplot(plot.pred[plot.pred$var != 'site.size',], aes(seq, pred)) + 
# 		geom_line(lwd=1.2, aes(col=var, linetype=var)) +
# 		scale_color_manual(values = c(cols[1], cols[1])) +
# 		scale_fill_manual(values = c(cols[1], cols[1])) +
# 		geom_ribbon(aes(ymin = lwr, ymax = upr, fill=var), alpha=0.2) +
# 		labs(x = '', y = expression(paste("g C ha"^-1,"min"^-1)), title='') +
# 		theme(legend.position = 'none', 
# 			plot.title = element_text(size=12, color='transparent')) +
# 		annotate('text', x = 49, y = 1, label='Macroalgae') +
# 		annotate('text', x = 46, y = 2.5, label='Available substrate')  +
# 		geom_rug(data=deciles, aes(macroalgae, 0.5), sides='b', alpha=1, col='grey50',size=1) +
# 		geom_rug(data=deciles, aes(substrate, 0.5), sides='t', alpha=1, col='grey50',size=1) + th

g1<-ggplot(plot.pred[!(plot.pred$var %in% c('macroalgae')),], aes(seq, pred)) + 
		geom_line(lwd=1.2, aes(col=var)) +
		scale_color_manual(values = c(cols[1], cols[1])) +
		scale_fill_manual(values = c(cols[1], cols[1])) +
		geom_ribbon(aes(ymin = lwr, ymax = upr, fill=var), alpha=0.2) +
		labs(x = '% cover', y = expression(paste("g C ha"^-1,"min"^-1)), title='Croppers') +
		theme(legend.position = 'none', 
			plot.title = element_text(size=18, vjust = 2.8,hjust =-0.15,  color='black')) +
		# annotate('text', x = 49, y = 1, label='Macroalgae') +
		annotate('text', x = 46, y = 2.5, label='Available substrate')  +
		# geom_rug(data=deciles, aes(macroalgae, 0.5), sides='b', alpha=1, col='grey50',size=1) +
		geom_rug(data=deciles, aes(substrate, 1), sides='b', alpha=1, col='grey50',size=1) + th

g3<-ggplot(plot.pred[!(plot.pred$var %in% c('substrate')),], aes(seq, pred)) + 
		geom_line(lwd=1.2, aes(col=var)) +
		scale_color_manual(values = c(cols[1], cols[1])) +
		scale_fill_manual(values = c(cols[1], cols[1])) +
		geom_ribbon(aes(ymin = lwr, ymax = upr, fill=var), alpha=0.2) +
		labs(x = '% cover', y = '', title='') +
		theme(legend.position = 'none', 
			plot.title = element_text(size=12, color='transparent')) +
		scale_y_continuous(breaks=seq(0.5, 1.8, 0.3), labels = scales::number_format(accuracy = 0.1)) +
		annotate('text', x = 40, y = 1, label='Macroalgae') +
		# annotate('text', x = 46, y = 2.5, label='Available substrate')  +
		geom_rug(data=deciles, aes(macroalgae, 0.5), sides='b', alpha=1, col='grey50',size=1) +
		# geom_rug(data=deciles, aes(substrate, 0.5), sides='t', alpha=1, col='grey50',size=1) + 
		th


# ## size effect for croppers
# g3<-ggplot(plot.pred[plot.pred$var == 'site.size',], aes(seq, pred)) + 
# 		geom_line(lwd=1.2, aes(col=var, linetype=var)) +
# 		scale_color_manual(values = c(cols[1], cols[1])) +
# 		scale_fill_manual(values = c(cols[1], cols[1])) +
# 		geom_ribbon(aes(ymin = lwr, ymax = upr, fill=var), alpha=0.2) +
# 		labs(x = 'Mean length, cm', y = '', title='') +
# 		theme(legend.position = 'none', 
# 			plot.title = element_text(size=12, color='transparent')) +
# 		geom_rug(data=deciles, aes(site.size, 1), sides='b', alpha=1, col='grey50',size=1) + th


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

plot.pred$var.lab <- factor(plot.pred$var, 
                        levels=c('crop','scrape'),
                        labels=c( expression(paste("g ha"^-1,"min"^-1)),
			 	 expression(paste('m'^2,'ha'^-1, 'min'^-1))))

deciles$var.lab <- unique(plot.pred$var.lab)[2]


g2A<-ggplot(plot.pred[plot.pred$var == 'crop',], aes(seq, pred)) + 
		geom_line(lwd=1.2, aes(col=var)) +
		geom_ribbon(aes(ymin = lwr, ymax = upr,fill=var), alpha=0.2) +
		scale_color_manual(values = cols.named) +
		scale_fill_manual(values = cols.named) +
		labs(x = 'Structural complexity', title = '') +
		theme(legend.position = 'none', 
			plot.title = element_text(size=14, vjust = 3,hjust =0.5,  color='black')) + 
		ylab(NULL) + th

g2B<-ggplot(plot.pred[plot.pred$var == 'scrape',], aes(seq, pred)) + 
		geom_line(lwd=1.2, aes(col=var)) +
		geom_ribbon(aes(ymin = lwr, ymax = upr,fill=var), alpha=0.2) +
		scale_color_manual(values = cols.named) +
		scale_fill_manual(values = cols.named) +
		labs(x = 'Structural complexity', title = '') +
		theme(legend.position = 'none', 
			plot.title = element_text(size=12, vjust = 3,hjust =0.5,  color='black')) + 
		scale_y_continuous(breaks=seq(0.5, 1, 0.1), labels = scales::number_format(accuracy = 0.1)) +
		ylab(NULL) +
		geom_rug(data=deciles, aes(complexity, 0.5), sides='b', alpha=1, col='grey50',size=1) + th


## plot substrate effects for scrapers
plot.pred<-scrape.pred %>% select(substrate) %>% gather(var, pred)
sub.seq<-with(scrape.raw, seq(min(substrate), max(substrate), length.out=100))
sub.var<-data.frame(mm.scrape[[3]]['substrate'] - mm.scrape[[4]]['substrate'], 
	mm.scrape[[3]]['substrate'] + mm.scrape[[4]]['substrate'], 'substrate')

plot.pred$seq<-c(sub.seq)
plot.pred$lwr<-c(sub.var[,1])
plot.pred$upr<-c(sub.var[,2])


g5<-ggplot(plot.pred, aes(seq, pred)) + 
		geom_line(lwd=1.2, col=cols[2], linetype=1) +
		geom_ribbon(aes(ymin = lwr, ymax = upr), fill=cols[2], alpha=0.2) +
		labs(x = '% cover', y = expression(paste('m'^2,'ha'^-1, 'min'^-1)), title='Scrapers') +
		theme(legend.position = 'none', 
			plot.title = element_text(size=18, vjust = 2.8,hjust =-0.15,  color='black')) +
		# annotate('text', x = 49, y = 1, label='Macroalgae') +
		annotate('text', x = 45, y = 0.7, label='Available substrate')  +
		scale_y_continuous(breaks=seq(0.5, 0.7, 0.05), labels = scales::number_format(accuracy = 0.05)) +
		# geom_rug(data=deciles, aes(macroalgae, 0.5), sides='b', alpha=1, col='grey50',size=1) +
		geom_rug(data=deciles, aes(substrate, 0.55), sides='b', alpha=1, col='grey50',size=1) + th


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
preds$cov<-factor(c('Fished', 'No-take', 'Remote'))
# preds$cov<-factor(preds$cov, levels = levels(preds$cov)[c(1,3,2)])

g4<-ggplot(preds, aes(cov, pred)) + geom_pointrange(col=cols[2],size=1.25, aes(ymin= lwr, ymax = upr)) +
	labs(x = '', y = '', title='') +
	# lims(y = c(0.5, 1.3)) +
	scale_y_continuous(breaks=seq(0.5, 1.3, 0.2), limits = c(0.5, 1.3), labels = scales::number_format(accuracy = 0.1)) +
	theme(legend.position = 'none') + th


pdf(file = 'figures/Figure3_predicted_effects.pdf', height = 6, width = 12)
left<-plot_grid(g1, g5, nrow=2, align = 'hv', rel_heights= c(0.5, 0.5), vjust=2,labels=c('A', 'D'),label_size=16)
mid<-plot_grid(g2A, g2B, nrow=2, align = 'hv', rel_heights= c(0.5, 0.5), vjust=2,labels=c('B', 'E'),label_size=16)
right<-plot_grid(g3, g4, nrow=2, align = 'hv', rel_heights=c(0.5, 0.5), vjust=2,labels=c('C', 'F'), label_size = 16)
plot_grid(left, mid, right, nrow=1, labels=c('', '', ''), label_size=0)

dev.off()