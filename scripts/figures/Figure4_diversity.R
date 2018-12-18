
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)
theme_set(theme_sleek())
setwd(here('grazing-gradients'))



# data load

## cropper data
load(file = 'results/models/function_m_croppers.Rdata'); h <- focal
load(file = 'results/cropper_attributes.Rdata')
h$site.richness.raw<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]

nd.rich<-data.frame(site.richness = seq(min(h$site.richness), max(h$site.richness), length.out=20),
              sp.richness.raw = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out=20),
                          hard.coral = 0, macroalgae = 0, rubble = 0, substrate = 0, complexity = 0, 
                          fish.biom = 0, fish.dummy = 0, pristine.dummy = 0, site.size =0,
                          dataset = 'GBR', reef='1')
nd.rich$pred<-predict(m.grazer, newdata=nd.rich, re.form=NA, type='response')

partials<-visreg::visreg(m.grazer, 'site.richness')
richness.fit.crop<-data.frame(fit=partials$fit$visregFit, x = partials$fit$site.richness, 
      lwr = partials$fit$visregLwr, upr = partials$fit$visregUpr,
  richness = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out = 101))

richness.points.crop<-data.frame(x=h$site.richness.raw, y = partials$res$visregRes, 
            dataset=h$dataset, management = h$management, biom=h$biom)

## scraping data
load(file = 'results/models/function_m_scrapers.Rdata'); h <- focal
load(file = 'results/scraper_attributes.Rdata')
h$site.richness.raw<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]

nd.rich<-data.frame(site.richness = seq(min(h$site.richness), max(h$site.richness), length.out=20),
              sp.richness.raw = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out=20),
                          hard.coral = 0, macroalgae = 0, rubble = 0, substrate = 0, complexity = 0, 
                          fish.biom = 0, fish.dummy = 0, pristine.dummy = 0, site.size =0,
                          dataset = 'GBR', reef='1')
nd.rich$pred<-predict(m.scraper, newdata=nd.rich, re.form=NA, type='response')

partials<-visreg::visreg(m.scraper, 'site.richness')
richness.fit.scrape<-data.frame(fit=partials$fit$visregFit, x = partials$fit$site.richness, 
    lwr = partials$fit$visregLwr, upr = partials$fit$visregUpr,
  richness = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out = 101))

richness.points.scrape<-data.frame(x=h$site.richness.raw, y = partials$res$visregRes, 
            dataset=h$dataset, management = h$management, biom=h$biom)



## add some plotting stuff
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12])


g1<-ggplot(richness.fit.crop, aes(richness, fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.1) + 
    geom_point(data=richness.points.crop, aes(x, y, shape = dataset), size=3.5, alpha=0.7, col=cols[1])  +
    labs(y = 'Partial effect on area grazed', x = 'Species richness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = c(0.8, 0.2)) +
    guides(size = F)


g2<-ggplot(richness.fit.scrape, aes(richness, fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.1) + 
    geom_point(data=richness.points.scrape, aes(x, y, shape = dataset), size=3.5, alpha=0.7, col=cols[2])  +
    labs(y = '', x = 'Species richness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none')




pdf(file='figures/Figure4_diversity.pdf', height = 4, width=9)
plot_grid(g1, g2, labels=c('a', 'b'))
dev.off()



