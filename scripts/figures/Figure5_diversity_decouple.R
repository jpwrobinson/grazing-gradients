
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


th<-theme(axis.text=element_text(size=12),
                axis.title=element_text(size=12))

# data load

## cropper data
load("results/models/cropper_function.Rdata")
croppers<-h
croppers$sp <- 'croppers'

load(file = 'results/cropper_attributes.Rdata')
croppers$site.richness<-diversity.preds$richness[match(croppers$unique.id, diversity.preds$unique.id)]
croppers$site.evenness<-diversity.preds$J[match(croppers$unique.id, diversity.preds$unique.id)]

## NOTE - site with 1 species dropped from analysis
# croppers$site.evenness[is.na(croppers$site.evenness)]<-1
croppers<- croppers[!is.na(croppers$site.evenness),]

## add rarefied vals
rare<-read.csv(file = 'results/rarefied_richness_croppers.csv')
croppers$site.rarefied<-rare$qD[match(croppers$unique.id, rare$site)]

crop.pred<-scaler(croppers, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))

m.graze<-glmer(cropping.gram.ha ~ biom +
                   site.rarefied +
                   site.evenness +
                   abund +
                   (1 | dataset/reef), crop.pred, family='Gamma'(link = 'log'))

# pairs2(dplyr::select_if(crop.pred, is.numeric), 
#   lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)

# m.table<-dredge(m.graze)
tab<-data.frame(m.table)
tab[is.na(tab)]<-0
write.csv(tab, 'results/tables/cropper_div_AICtable.csv')

## MMI for decoupling
# mm.crop<-mmi_tvalue(m.graze, dataset = crop.pred,  
#     exp.names = c('biom', 'site.rarefied', 'site.evenness', 'abund'), 
#     indicator = 'cropping.gram.ha', 
#     ranef = c('dataset', 'reef'), family='Gamma')
r<-mm.crop[[2]]
r
# r2 top model = 0.86 marg,  0.86 cond

## top model is:
crop.top<-glmer(cropping.gram.ha ~ biom +
                   #site.rarefied +
                   site.evenness +
                   abund +
                   (1 | dataset/reef), crop.pred, family='Gamma'(link = 'log'))


load("results/models/scraper_function.Rdata")
scrapers<-h
scrapers$sp <- 'scrapers'

load(file = 'results/scraper_attributes.Rdata')
scrapers$site.richness<-diversity.preds$richness[match(scrapers$unique.id, diversity.preds$unique.id)]
scrapers$site.evenness<-diversity.preds$J[match(scrapers$unique.id, diversity.preds$unique.id)]
scrapers$site.evenness[is.na(scrapers$site.evenness)]<-0
rare<-read.csv(file = 'results/rarefied_richness_scrapers.csv')
scrapers$site.rarefied<-rare$qD[match(scrapers$unique.id, rare$site)]

scrap.pred<-scaler(scrapers, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'scraping'))

m.graze<-glmer(scraping ~ biom +
                   site.rarefied +
                   site.evenness +
                   abund +
                   (1 | dataset/reef), scrap.pred, family='Gamma'(link = 'log'))

pairs2(dplyr::select_if(scrap.pred, is.numeric), 
  lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)

# m.table<-dredge(m.graze)
tab<-data.frame(m.table)
tab[is.na(tab)]<-0

write.csv(tab, 'results/tables/scraper_div_AICtable.csv')

## MMI for decopuling
# mm.scrap<-mmi_tvalue(m.graze, dataset = scrap.pred,  
#     exp.names = c('biom', 'site.rarefied', 'site.evenness', 'abund'), 
#     indicator = 'scraping', 
#     ranef = c('dataset', 'reef'), family='Gamma')



## top model is:
scrap.top<-glmer(scraping ~ biom +
                   site.rarefied +
                   site.evenness +
                   abund +
                   (1 | dataset/reef), scrap.pred, family='Gamma'(link = 'log'))


## cropping evenness
nd.even.crop<-data.frame(site.evenness = seq(min(crop.pred$site.evenness), max(crop.pred$site.evenness), length.out=20),
              site.evenness.raw = seq(min(croppers$site.evenness), max(croppers$site.evenness), length.out=20),
              biom = 0, abund = 0,
                          dataset = 'GBR', reef='1')
nd.even.crop$pred<-predict(crop.top, newdata=nd.even.crop, re.form=NA, type='response')
nd.even.crop$se<-predict(crop.top, newdata=nd.even.crop, re.form=NA, type='response', se.fit=TRUE)$se.fit

crop.raw.even<-croppers %>% group_by(reef, dataset) %>% summarise(x = mean(site.evenness), y = mean(cropping.gram.ha)) 

## scraping richness
nd.rich.scrape<-data.frame(site.rarefied = seq(min(scrap.pred$site.rarefied), max(scrap.pred$site.rarefied), length.out=20),
              site.rarefied.raw = seq(min(scrapers$site.rarefied), max(scrapers$site.rarefied), length.out=20),
                          biom=0, abund = 0, site.evenness= 0 ,
                          dataset = 'GBR', reef='1')
nd.rich.scrape$pred<-predict(scrap.top, newdata=nd.rich.scrape, re.form=NA, type='response')
nd.rich.scrape$se<-predict(scrap.top, newdata=nd.rich.scrape, re.form=NA, type='response', se.fit=TRUE)$se.fit

scrap.raw.rich<-scrapers %>% group_by(reef, dataset) %>% summarise(x = mean(site.rarefied), y = mean(scraping)) 

## scraping evenness
nd.even.scrape<-data.frame(site.evenness = seq(min(scrap.pred$site.evenness), max(scrap.pred$site.evenness), length.out=20),
              site.evenness.raw = seq(min(scrapers$site.evenness), max(scrapers$site.evenness), length.out=20),
                          biom=0, abund = 0, site.rarefied= 0 ,
                          dataset = 'GBR', reef='1')
nd.even.scrape$pred<-predict(scrap.top, newdata=nd.even.scrape, re.form=NA, type='response')
nd.even.scrape$se<-predict(scrap.top, newdata=nd.even.scrape, re.form=NA, type='response', se.fit=TRUE)$se.fit

scrap.raw.even<-scrapers %>% group_by(reef, dataset) %>% summarise(x = mean(site.evenness), y = mean(scraping)) 

## add some plotting stuff
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12])


### predicted relationships
g1<-ggplot(nd.even.crop) + 
    geom_ribbon(aes(site.evenness.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1, fill=cols[1]) + 
    geom_line(aes(site.evenness.raw, pred), col=cols[1]) +
    labs(y = expression(paste("g ha"^-1,"min"^-1)),
     x = 'Species evenness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = c(0.58, 0.95),
          # legend.spacing.x = unit(0, 'cm'),
          # legend.spacing.y = unit(0, 'cm'),
          legend.key.size = unit(0.5, "cm"),
          # legend.box.background = element_rect(colour = "black"),
           legend.text=element_text(size=9)) +
    guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
    geom_point(data=croppers, size=3, col=cols[1], aes(site.evenness, cropping.gram.ha, shape=dataset)) +th


g2<-ggplot(nd.rich.scrape) + 
    geom_ribbon(aes(site.rarefied.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1,fill=cols[2]) + 
    geom_line(aes(site.rarefied.raw, pred), col=cols[2]) +
    # geom_point(data=h, aes(site.rarefied, scraping, shape = dataset), size=3.5, alpha=0.7, col=cols[2])  +
    labs(y = expression(paste('m'^2,' ha'^-1, 'min'^-1)),
     x = 'Species richness (rarefied)') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none') +
    geom_point(data=scrapers,  size=3, col=cols[2],aes(site.rarefied, scraping, shape=dataset)) +th


g3<-ggplot(nd.even.scrape) + 
    geom_ribbon(aes(site.evenness.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1,fill=cols[2]) + 
    geom_line(aes(site.evenness.raw, pred), col=cols[2]) +
    # geom_point(data=h, aes(site.rarefied, scraping, shape = dataset), size=3.5, alpha=0.7, col=cols[2])  +
    labs(y = expression(paste('m'^2,' ha'^-1, 'min'^-1)),
     x = 'Species evenness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none') +
    geom_point(data=scrapers, size=3,  col=cols[2],aes(site.evenness, scraping, shape=dataset)) +th




pdf(file='figures/Figure5_diversity.pdf', height = 3, width=12)
plot_grid(g1, g2, g3, labels=c('A', 'B', 'C'), nrow=1)
dev.off()


