
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
library(MuMIn); options(na.action = 'na.fail')


# data load

## cropper data
load("results/models/cropper_function.Rdata")
croppers<-h
croppers$sp <- 'croppers'

load(file = 'results/cropper_attributes.Rdata')
croppers$site.richness<-diversity.preds$richness[match(croppers$unique.id, diversity.preds$unique.id)]
croppers$site.evenness<-diversity.preds$J[match(croppers$unique.id, diversity.preds$unique.id)]
croppers$site.evenness[is.na(croppers$site.evenness)]<-0
rare<-read.csv(file = 'results/rarefied_richness_croppers.csv')
croppers$site.rarefied<-rare$qD[match(croppers$unique.id, rare$site)]

crop.pred<-scaler(croppers, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))

m.graze<-glmer(cropping.gram.ha ~ biom +
                   site.rarefied +
                   site.evenness +
                   abund +
                   (1 | dataset/reef), crop.pred, family='Gamma'(link = 'log'))
colnames(m.graze)
pairs2(dplyr::select_if(crop.pred, is.numeric), 
  lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)


debug(mmi_tvalue)
mm.crop<-mmi_tvalue(m.graze, dataset = crop.pred,  
    exp.names = c('biom', 'site.rarefied', 'site.evenness', 'abund'), 
    indicator = 'cropping.gram.ha', 
    ranef = c('dataset', 'reef'), family='Gamma')

load("results/models/scraper_function.Rdata")
scrapers<-h
scrapers$grazef<-scrapers$scraping
scrapers$scraping<-NULL
scrapers$sp <- 'scrapers'

m.scrape<-glmer(scraping ~ scale(biom) + (1 | dataset/reef), h, family='Gamma'(link = 'log'))


nd.rich.crop<-data.frame(site.richness = seq(min(h$site.richness), max(h$site.richness), length.out=20),
              sp.richness.raw = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out=20),
                          hard.coral = 0, macroalgae = 0, rubble = 0, substrate = 0, complexity = 0, 
                          fish.biom = 0, fish.dummy = 0, pristine.dummy = 0, site.size =0,
                          dataset = 'GBR', reef='1')
nd.rich.crop$pred<-predict(m.grazer, newdata=nd.rich.crop, re.form=NA, type='response')
nd.rich.crop$se<-predict(m.grazer, newdata=nd.rich.crop, re.form=NA, type='response', se.fit=TRUE)$se.fit

partials<-visreg::visreg(m.grazer, 'site.richness')
richness.fit.crop<-data.frame(fit=partials$fit$visregFit, x = partials$fit$site.richness, 
      lwr = partials$fit$visregLwr, upr = partials$fit$visregUpr,
  richness = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out = 101))

richness.points.crop<-data.frame(x=h$site.richness.raw, y = partials$res$visregRes, 
            dataset=h$dataset, management = h$management, biom=h$biom)

crop<-h

## scraping data
load(file = 'results/models/function_m_scrapers.Rdata'); h <- focal
load(file = 'results/scraper_attributes.Rdata')
h$site.richness.raw<-diversity.preds$richness[match(h$unique.id, diversity.preds$unique.id)]

nd.rich.scrape<-data.frame(site.richness = seq(min(h$site.richness), max(h$site.richness), length.out=20),
              sp.richness.raw = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out=20),
                          hard.coral = 0, macroalgae = 0, rubble = 0, substrate = 0, complexity = 0, 
                          fish.biom = 0, fish.dummy = 0, pristine.dummy = 0, site.size =0,
                          dataset = 'GBR', reef='1')
nd.rich.scrape$pred<-predict(m.scraper, newdata=nd.rich.scrape, re.form=NA, type='response')
nd.rich.scrape$se<-predict(m.grazer, newdata=nd.rich.scrape, re.form=NA, type='response', se.fit=TRUE)$se.fit

partials<-visreg::visreg(m.scraper, 'site.richness')
richness.fit.scrape<-data.frame(fit=partials$fit$visregFit, x = partials$fit$site.richness, 
    lwr = partials$fit$visregLwr, upr = partials$fit$visregUpr,
  richness = seq(min(h$site.richness.raw), max(h$site.richness.raw), length.out = 101))

richness.points.scrape<-data.frame(x=h$site.richness.raw, y = partials$res$visregRes, 
            dataset=h$dataset, management = h$management, biom=h$biom)



## add some plotting stuff
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12])


### predicted relationships
g1<-ggplot(nd.rich.crop, aes(sp.richness.raw, pred)) + 
    geom_ribbon(aes(ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1) + 
    geom_line() +
    geom_point(data=crop, aes(site.richness.raw, cropping.gram.ha, shape = dataset), size=3.5, alpha=0.7, col=cols[1])  +
    labs(y = 'Partial effect on area grazed', x = 'Species richness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = c(0.2, 0.8)) +
    guides(size = F)


g2<-ggplot(nd.rich.scrape, aes(sp.richness.raw, pred)) + 
    geom_ribbon(aes(ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1) + 
    geom_line() +
    geom_point(data=h, aes(site.richness.raw, scraping, shape = dataset), size=3.5, alpha=0.7, col=cols[2])  +
    labs(y = '', x = 'Species richness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none')




pdf(file='figures/Figure4_diversity.pdf', height = 4, width=9)
plot_grid(g1, g2, labels=c('a', 'b'))
dev.off()



