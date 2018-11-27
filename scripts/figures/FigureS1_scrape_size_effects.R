
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(rethinking)

setwd(here('grazing-gradients'))

pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[1], pal[8], pal[12], pal[20])

cols.named<-c('Cetoscarus' = cols[1], 'Chlorurus' = cols[2], 'Hipposcarus' = cols[3], 'Scarus' = cols[4])
theme_set(theme_sleek())


## evaluating predictions for each species
load('results/models/bites_scrapers.Rdata')

## size effects - bite rate
d.pred <- data.frame(Genus = 'NA', sp = 'NA', TL = seq(min(scrapers$TL), max(scrapers$TL), length.out =30), dataset = 'NA')
a.sp.zero = matrix(0, 1000, 28); a.genus.zero = matrix(0, 1000, 4); a.dataset.zero = matrix(0, 1000, 2)
link.obs.size<-link(scrape.m, n = 1000, data = as.list(d.pred), 
                      replace= list(X1= a.sp.zero, X2 = a.genus.zero, X3 = a.dataset.zero))
pred.mean.size<-data.frame(median = apply(link.obs.size, 2, median))
pred.PI<-apply(link.obs.size, 2, PI, prob = 0.95)
pred.mean.size$upper<-pred.PI[2,]
pred.mean.size$lower<-pred.PI[1,]
pred.mean.size$TL<-d.pred$TL


g1<-ggplot(pred.mean.size, aes(TL, median)) + 
	geom_point(data = scrapers, aes(TL, biterate, col=Genus), size=1, alpha=0.4) +
		geom_line() + 
      geom_line(data = pred.mean.size, aes(TL, upper), linetype='dotted') +
      geom_line(data = pred.mean.size, aes(TL, lower), linetype='dotted') + 
      labs(x = 'Total length, cm', y = expression('Bites minute'^-1), parse=T) +
      theme(legend.position = c(0.8, 0.8), 
      			legend.title=element_blank(),
      			legend.key.size = unit(8, 'point')) +
      scale_color_manual(values = cols.named) +
      guides(color = guide_legend(override.aes = list(size=5, alpha=1)))


## size effects - scrape area
pred.mean.size<- read.csv(file = 'results/functions/scraper_area_predicted.csv')
load('results/models/area_scrapers.Rdata')


g2<-ggplot(pred.mean.size, aes(TL, median)) + 
		geom_point(data=area, aes(TL, bitearea, col=genus), alpha=0.4, size=1) +
		geom_line() + 
      geom_line(data = pred.mean.size, aes(TL, upper), linetype='dotted') +
      geom_line(data = pred.mean.size, aes(TL, lower), linetype='dotted') + 
      labs(x = 'Total length, cm', y = expression('Area scraped, mm'^2), parse=TRUE) +
      scale_color_manual(values = cols.named) +
      theme(legend.position='none')

pdf(file='figures/FigureS1_scrape_size.pdf', height=5, width=10)
cowplot::plot_grid(g1, g2, labels=c('a', 'b'))
dev.off()