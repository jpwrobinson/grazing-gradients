
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)

setwd(here('grazing-gradients'))
theme_set(theme_sleek())

## get cols
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[1], pal[8], pal[12], pal[20])
cols.named<-c('Chagos' = cols[1], 'GBR' = cols[2], 'Maldives'=cols[3], 'Seychelles' = cols[4])


p<-read.csv(file = 'results/functions/grazer_bites_predicted.csv')
p<-p[!p$class == 'global.mean',]
p<-p[order(p$preds, p$class),]

g1<-ggplot(p, aes(factor(class, levels = rev(levels(class))), median)) + 
			geom_pointrange(aes(ymin = lower, ymax = upper),col=pal[5]) + 
			coord_flip() + 
			theme(legend.position = 'NULL') + 
			labs( x = '', y = expression('Bites minute'^-1), parse=T) + 
			facet_wrap(~preds, scales='free_y')



load(file = 'results/models/cropper_function.Rdata')

g2<-ggplot(h, aes(reorder(reef, cropping.gram.ha, FUN=median), cropping.gram.ha, fill=dataset)) + 
			geom_boxplot() + 
			xlab('') + 
			ylab(expression(paste("algal consumption, g C ha"^-1,"min"^-1))) +
			scale_fill_manual(values = cols.named) +
			theme(legend.position = c(0.8, 0.2),
					legend.title=element_blank()) +
			coord_flip()


pdf(file='figures/FigureS2_cropper_bites.pdf', height=5, width=10)
plot_grid(g1, g2, labels=c('A', 'B'), nrow=1)
dev.off()