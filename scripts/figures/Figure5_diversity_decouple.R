
library(tidyverse)
library(cowplot)
library(ggplot2)
library(visreg)
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

## add beta estimates
load('results/graze_beta_estimates.Rdata')
croppers$site.beta.rich<-g.diversity$beta_rich[match(croppers$unique.id, g.diversity$site)]
croppers$site.beta.repl<-g.diversity$beta_repl[match(croppers$unique.id, g.diversity$site)]
croppers$site.beta<-g.diversity$beta[match(croppers$unique.id, g.diversity$site)]


crop.pred<-scaler(croppers, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))

m.graze<-glmer(cropping.gram.ha ~ biom +
                   site.rarefied +
                   # site.evenness +
                   # site.beta.repl +
                   # site.beta.rich +
                   site.beta +
                   abund +
                   (1 | dataset/reef), crop.pred, family='Gamma'(link = 'log'))
car::vif(m.graze)

pairs2(dplyr::select_if(crop.pred, is.numeric), 
  lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)

# m.table<-dredge(m.graze)
tab<-data.frame(m.table)
tab[is.na(tab)]<-0
write.csv(tab, 'results/tables/cropper_div_AICtable.csv')

## MMI for decoupling
mm.crop<-mmi_tvalue(m.graze, dataset = crop.pred,  
    exp.names = c('biom', 'site.rarefied', 'site.evenness','site.beta.rich', 'abund'), 
    indicator = 'cropping.gram.ha', 
    ranef = c('dataset', 'reef'), family='Gamma')
r<-mm.crop[[2]]
r
# r2 top model = 0.86 marg,  0.87 cond

## top model is:
crop.top<-glmer(cropping.gram.ha ~ biom +
                   site.rarefied +
                   # site.evenness +
                   # site.beta.rich +
                   site.beta + 
                   # site.beta.repl +
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

scrapers$site.beta.rich<-s.diversity$beta_rich[match(scrapers$unique.id, s.diversity$site)]
scrapers$site.beta.repl<-s.diversity$beta_repl[match(scrapers$unique.id, s.diversity$site)]
scrapers$site.beta<-s.diversity$beta[match(scrapers$unique.id, s.diversity$site)]



scrap.pred<-scaler(scrapers, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'scraping'))

m.scrape<-glmer(scraping ~ biom +
                   site.rarefied +
                   # site.evenness +
                   # site.beta.rich +
                   site.beta +
                   # site.beta.repl +
                   abund +
                   (1 | dataset/reef), scrap.pred, family='Gamma'(link = 'log'))
car::vif(m.scrape)
pairs2(dplyr::select_if(scrap.pred, is.numeric), 
  lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)

m.table<-dredge(m.scrape)
tab<-data.frame(m.table)
tab[is.na(tab)]<-0

write.csv(tab, 'results/tables/scraper_div_AICtable.csv')

## visreg for partial resids
graze.regs<-visreg::visreg(m.scrape)
str(graze.regs)

## MMI for decopuling
mm.scrap<-mmi_tvalue(m.scrape, dataset = scrap.pred,  
    exp.names = c('biom', 'site.rarefied', 'site.evenness','site.beta.rich', 'abund'), 
    indicator = 'scraping', 
    ranef = c('dataset', 'reef'), family='Gamma')

## top model is:
scrap.top<-glmer(scraping ~ biom +
                   site.rarefied +
                   # site.evenness +
                   # site.beta.rich +
                   site.beta +
                   # site.beta.repl +
                   abund +
                   (1 | dataset/reef), scrap.pred, family='Gamma'(link = 'log'))

save(scrap.top, crop.top, file = 'results/models/decouple_biodiv_mods.Rdata')

## cropping richness
nd.rich.crop<-data.frame(site.rarefied = seq(min(crop.pred$site.rarefied), max(crop.pred$site.rarefied), length.out=20),
              site.rarefied.raw = seq(min(croppers$site.rarefied), max(croppers$site.rarefied), length.out=20),
                          biom=0, abund = 0, site.evenness= 0 ,site.beta.rich = 0,site.beta.repl = 0,site.beta=0,
                          dataset = 'GBR', reef='1')
nd.rich.crop$pred<-predict(scrap.top, newdata=nd.rich.crop, re.form=NA, type='response')
nd.rich.crop$se<-predict(scrap.top, newdata=nd.rich.crop, re.form=NA, type='response', se.fit=TRUE)$se.fit

crop.raw.rich<-croppers %>% group_by(reef, dataset) %>% summarise(x = mean(site.rarefied), y = mean(cropping.gram.ha)) 

pm<-visreg(crop.top, 'site.rarefied', type='contrast', scale='response', partial=TRUE, plot=T)
rich.crop.fit<-pm$fit
rich.crop.res<-pm$res
rich.crop.fit$x<-rich.crop.fit$site.rarefied * sd(croppers$site.rarefied) + mean(croppers$site.rarefied)
rich.crop.res$x<-rich.crop.res$site.rarefied * sd(croppers$site.rarefied) + mean(croppers$site.rarefied)



## cropping evenness
nd.even.crop<-data.frame(site.evenness = seq(min(crop.pred$site.evenness), max(crop.pred$site.evenness), length.out=20),
              site.evenness.raw = seq(min(croppers$site.evenness), max(croppers$site.evenness), length.out=20),
              biom = 0, abund = 0,site.beta.rich = 0,site.rarefied = 0,site.beta.repl = 0,site.beta=0,
                          dataset = 'GBR', reef='1')
nd.even.crop$pred<-predict(crop.top, newdata=nd.even.crop, re.form=NA, type='response')
nd.even.crop$se<-predict(crop.top, newdata=nd.even.crop, re.form=NA, type='response', se.fit=TRUE)$se.fit


pm<-visreg(crop.top, 'site.evenness', type='contrast', scale='response', partial=TRUE, plot=T)
even.crop.fit<-pm$fit
even.crop.res<-pm$res
even.crop.fit$x<-even.crop.fit$site.evenness * sd(croppers$site.evenness) + mean(croppers$site.evenness)
even.crop.res$x<-even.crop.res$site.evenness * sd(croppers$site.evenness) + mean(croppers$site.evenness)


crop.raw.even<-croppers %>% group_by(reef, dataset) %>% summarise(x = mean(site.evenness), y = mean(cropping.gram.ha)) 


## cropping beta
nd.beta.crop<-data.frame(site.beta = seq(min(crop.pred$site.beta), max(crop.pred$site.beta), length.out=20),
              site.beta.raw = seq(min(croppers$site.beta), max(croppers$site.beta), length.out=20),
              biom = 0, abund = 0,site.beta.rich = 0,site.rarefied = 0,site.beta.repl = 0,site.beta.rich=0,
                          dataset = 'GBR', reef='1')
nd.beta.crop$pred<-predict(crop.top, newdata=nd.beta.crop, re.form=NA, type='response')
nd.beta.crop$se<-predict(crop.top, newdata=nd.beta.crop, re.form=NA, type='response', se.fit=TRUE)$se.fit


pm<-visreg(crop.top, 'site.beta', type='contrast', scale='response', partial=TRUE, plot=T)
beta.crop.fit<-pm$fit
beta.crop.res<-pm$res
beta.crop.fit$x<-beta.crop.fit$site.beta * sd(scrapers$site.beta) + mean(scrapers$site.beta)
beta.crop.res$x<-beta.crop.res$site.beta * sd(scrapers$site.beta) + mean(scrapers$site.beta)


crop.raw.beta<-croppers %>% group_by(reef, dataset) %>% summarise(x = mean(site.beta), y = mean(cropping.gram.ha)) 


## scraping richness
nd.rich.scrape<-data.frame(site.rarefied = seq(min(scrap.pred$site.rarefied), max(scrap.pred$site.rarefied), length.out=20),
              site.rarefied.raw = seq(min(scrapers$site.rarefied), max(scrapers$site.rarefied), length.out=20),
                          biom=0, abund = 0, site.evenness= 0 ,site.beta.rich = 0,site.beta.repl = 0,site.beta=0,
                          dataset = 'GBR', reef='1')
nd.rich.scrape$pred<-predict(scrap.top, newdata=nd.rich.scrape, re.form=NA, type='response')
nd.rich.scrape$se<-predict(scrap.top, newdata=nd.rich.scrape, re.form=NA, type='response', se.fit=TRUE)$se.fit


pm<-visreg(scrap.top, 'site.rarefied', type='contrast', scale='response', partial=TRUE, plot=T)
rich.scrape.fit<-pm$fit
rich.scrape.res<-pm$res
rich.scrape.fit$x<-rich.scrape.fit$site.rarefied * sd(scrapers$site.rarefied) + mean(scrapers$site.rarefied)
rich.scrape.res$x<-rich.scrape.res$site.rarefied * sd(scrapers$site.rarefied) + mean(scrapers$site.rarefied)


scrap.raw.rich<-scrapers %>% group_by(reef, dataset) %>% summarise(x = mean(site.rarefied), y = mean(scraping)) 

## scraping evenness
nd.even.scrape<-data.frame(site.evenness = seq(min(scrap.pred$site.evenness), max(scrap.pred$site.evenness), length.out=20),
              site.evenness.raw = seq(min(scrapers$site.evenness), max(scrapers$site.evenness), length.out=20),
                          biom=0, abund = 0, site.rarefied= 0 ,site.beta.rich = 0,site.beta.repl = 0,site.beta=0,
                          dataset = 'GBR', reef='1')
nd.even.scrape$pred<-predict(scrap.top, newdata=nd.even.scrape, re.form=NA, type='response')
nd.even.scrape$se<-predict(scrap.top, newdata=nd.even.scrape, re.form=NA, type='response', se.fit=TRUE)$se.fit


pm<-visreg(scrap.top, 'site.evenness', type='contrast', scale='response', partial=TRUE, plot=T)
even.scrape.fit<-pm$fit
even.scrape.res<-pm$res
even.scrape.fit$x<-even.scrape.fit$site.evenness * sd(scrapers$site.evenness) + mean(scrapers$site.evenness)
even.scrape.res$x<-even.scrape.res$site.evenness * sd(scrapers$site.evenness) + mean(scrapers$site.evenness)


scrap.raw.even<-scrapers %>% group_by(reef, dataset) %>% summarise(x = mean(site.evenness), y = mean(scraping)) 

## scraping beta
nd.beta.scrape<-data.frame(site.beta = seq(min(scrap.pred$site.beta), max(scrap.pred$site.beta), length.out=20),
              site.beta.raw = seq(min(scrapers$site.beta), max(scrapers$site.beta), length.out=20),
                          biom=0, abund = 0, site.rarefied= 0 ,site.evenness = 0,site.beta.repl = 0,site.beta.rich=0,
                          dataset = 'GBR', reef='1')
nd.beta.scrape$pred<-predict(scrap.top, newdata=nd.beta.scrape, re.form=NA, type='response')
nd.beta.scrape$se<-predict(scrap.top, newdata=nd.beta.scrape, re.form=NA, type='response', se.fit=TRUE)$se.fit


pm<-visreg(scrap.top, 'site.beta', type='contrast', scale='response', partial=TRUE, plot=T)
beta.scrape.fit<-pm$fit
beta.scrape.res<-pm$res
beta.scrape.fit$x<-beta.scrape.fit$site.beta * sd(scrapers$site.beta) + mean(scrapers$site.beta)
beta.scrape.res$x<-beta.scrape.res$site.beta * sd(scrapers$site.beta) + mean(scrapers$site.beta)


scrap.raw.beta<-scrapers %>% group_by(reef, dataset) %>% summarise(x = mean(site.beta), y = mean(scraping)) 


save(
  rich.crop.fit,
  beta.crop.fit,
  rich.scrape.fit,
  beta.scrape.fit, 
  file = 'results/models/decouple_biodiv_preds.Rdata')

## add some plotting stuff
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12])


# ### predicted relationships
# g0<-ggplot(nd.rich.crop) + 
#     geom_ribbon(aes(site.rarefied.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1, fill=cols[1]) + 
#     geom_line(aes(site.rarefied.raw, pred), col=cols[1]) +
#     labs(y = expression(paste("g ha"^-1,"min"^-1)),
#      x = '') +
#     # geom_hline(yintercept=0, linetype=5, col='grey') +
#     theme(legend.title=element_blank(),
#           legend.position = 'none',
#           # legend.spacing.x = unit(0, 'cm'),
#           # legend.spacing.y = unit(0, 'cm'),
#           legend.key.size = unit(0.5, "cm"),
#           plot.margin = unit(c(0.25, 0.25, 0, 0.25), "cm"),
#           # legend.box.background = element_rect(colour = "black"),
#            legend.text=element_text(size=9)) +
#     guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
#     geom_point(data=croppers, size=2, col=cols[1], aes(site.rarefied, cropping.gram.ha, shape=dataset)) +th


# g1<-ggplot(nd.even.crop) + 
#     geom_ribbon(aes(site.evenness.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1, fill=cols[1]) + 
#     geom_line(aes(site.evenness.raw, pred), col=cols[1]) +
#     labs(y = '',x = '') +
#     # geom_hline(yintercept=0, linetype=5, col='grey') +
#     theme(legend.title=element_blank(),
#           legend.position = 'none',
#           # legend.spacing.x = unit(0, 'cm'),
#           # legend.spacing.y = unit(0, 'cm'),
#           legend.key.size = unit(0.5, "cm"),
#           plot.margin = unit(c(0.25, 0.25, 0, 0.25), "cm"),
#           # legend.box.background = element_rect(colour = "black"),
#            legend.text=element_text(size=9)) +
#     guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
#     geom_point(data=croppers, size=2, col=cols[1], aes(site.evenness, cropping.gram.ha, shape=dataset)) +th


# g2<-ggplot(nd.beta.crop) + 
#     geom_ribbon(aes(site.beta.rich.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1, fill=cols[1]) + 
#     geom_line(aes(site.beta.rich.raw, pred), col=cols[1]) +
#     labs(y = '',x = '') +
#     # geom_hline(yintercept=0, linetype=5, col='grey') +
#     theme(legend.title=element_blank(),
#           legend.position = 'none',
#           # legend.spacing.x = unit(0, 'cm'),
#           # legend.spacing.y = unit(0, 'cm'),
#           legend.key.size = unit(0.5, "cm"),
#           plot.margin = unit(c(0.25, 0.25, 0, 0.25), "cm"),
#           # legend.box.background = element_rect(colour = "black"),
#            legend.text=element_text(size=9)) +
#     guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
#     geom_point(data=croppers, size=2, col=cols[1], aes(site.beta.rich, cropping.gram.ha, shape=dataset)) +th



# g3<-ggplot(nd.rich.scrape) + 
#     geom_ribbon(aes(site.rarefied.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1,fill=cols[2]) + 
#     geom_line(aes(site.rarefied.raw, pred), col=cols[2]) +
#     # geom_point(data=h, aes(site.rarefied, scraping, shape = dataset), size=2.5, alpha=0.7, col=cols[2])  +
#     labs(y = expression(paste('m'^2,' ha'^-1, 'min'^-1)),
#      x = 'Species richness') +
#     # geom_hline(yintercept=0, linetype=5, col='grey') +
#     theme(legend.title=element_blank(),
#       plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
#           legend.position = 'none') +
#     geom_point(data=scrapers, size=2, col=cols[2],aes(site.rarefied, scraping, shape=dataset)) +th


# g4<-ggplot(nd.even.scrape) + 
#     geom_ribbon(aes(site.evenness.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1,fill=cols[2]) + 
#     geom_line(aes(site.evenness.raw, pred), col=cols[2]) +
#     # geom_point(data=h, aes(site.rarefied, scraping, shape = dataset), size=2.5, alpha=0.7, col=cols[2])  +
#     labs(y = '',
#      x = 'Species evenness') +
#     # geom_hline(yintercept=0, linetype=5, col='grey') +
#     theme(legend.title=element_blank(),
#       plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
#           legend.position = 'none') +
#     geom_point(data=scrapers, size=2,  col=cols[2],aes(site.evenness, scraping, shape=dataset)) +th

# g5<-ggplot(nd.beta.scrape) + 
#     geom_ribbon(aes(site.beta.rich.raw, pred, ymin = pred - 2*se, ymax = pred + 2*se), alpha=0.1, fill=cols[1]) + 
#     geom_line(aes(site.beta.rich.raw, pred), col=cols[2]) +
#     labs(y = '',
#      x = expression(paste(beta,'-diversity'))) +
#     # geom_hline(yintercept=0, linetype=5, col='grey') +
#     theme(legend.title=element_blank(),
#           legend.position = c(0.8, 0.75),
#           # legend.spacing.x = unit(0, 'cm'),
#           # legend.spacing.y = unit(0, 'cm'),
#           legend.key.size = unit(0.5, "cm"),
#           plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
#           # legend.box.background = element_rect(colour = "black"),
#            legend.text=element_text(size=9)) +
#     guides(size = F, shape=guide_legend(nrow=4,byrow=TRUE,override.aes = list(col='black', size=2))) +
#     geom_point(data=scrapers, size=2, col=cols[2], aes(site.beta.rich, scraping, shape=dataset)) +th



# pdf(file='figures/Figure5_diversity_alt.pdf', height = 5, width=8)
# # plot_grid(g0, g1, g2, g3, g4, g5, labels=c('A', 'B', 'C', 'D' ,'E', 'F'), nrow=2)
# plot_grid(g0, g2, g3, g5, labels=c('A', 'B', 'C', 'D'), nrow=2)
# dev.off()




### predicted relationships
g0<-ggplot(rich.crop.fit) + 
    geom_ribbon(aes(x, visregFit, ymin = visregLwr, ymax = visregUpr), alpha=0.1,fill=cols[1]) + 
    geom_line(aes(x, visregFit), col=cols[1]) +
    labs(y = expression(paste("g ha"^-1,"min"^-1)),
     x = '') +
    scale_x_continuous(breaks=seq(2,10, 2)) +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none',
          # legend.spacing.x = unit(0, 'cm'),
          # legend.spacing.y = unit(0, 'cm'),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = unit(c(0.25, 0.25, 0, 0.25), "cm"),
          # legend.box.background = element_rect(colour = "black"),
           legend.text=element_text(size=9)) +
    guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
    geom_point(data=rich.crop.res, size=2,  col=cols[1],aes(x, visregRes, shape=dataset)) +th


g1<-ggplot(even.crop.fit) + 
    geom_ribbon(aes(x, visregFit, ymin = visregLwr, ymax = visregUpr), alpha=0.1,fill=cols[1]) + 
    geom_line(aes(x, visregFit), col=cols[1]) +
    labs(y = '',
     x = '') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none',
          # legend.spacing.x = unit(0, 'cm'),
          # legend.spacing.y = unit(0, 'cm'),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = unit(c(0.25, 0.25, 0, 0.25), "cm"),
          # legend.box.background = element_rect(colour = "black"),
           legend.text=element_text(size=9)) +
    guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
    geom_point(data=even.crop.res, size=2,  col=cols[1],aes(x, visregRes, shape=dataset)) +th


g2<-ggplot(beta.crop.fit) + 
    geom_ribbon(aes(x, visregFit, ymin = visregLwr, ymax = visregUpr), alpha=0.1,fill=cols[1]) + 
    geom_line(aes(x, visregFit), col=cols[1]) +
    labs(y = '',x = '') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none',
          # legend.spacing.x = unit(0, 'cm'),
          # legend.spacing.y = unit(0, 'cm'),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = unit(c(0.25, 0.25, 0, 0.25), "cm"),
          # legend.box.background = element_rect(colour = "black"),
           legend.text=element_text(size=9)) +
    guides(size = F, shape=guide_legend(nrow=1,byrow=TRUE,override.aes = list(col='black', size=2))) +
    geom_point(data=beta.crop.res, size=2,  col=cols[1],aes(x, visregRes, shape=dataset)) +th



g3<-ggplot(rich.scrape.fit) + 
    geom_ribbon(aes(x, visregFit, ymin = visregLwr, ymax = visregUpr), alpha=0.1,fill=cols[2]) + 
    geom_line(aes(x, visregFit), col=cols[2]) +
    scale_x_continuous(breaks=seq(3, 12, 3)) +
    # geom_point(data=h, aes(site.rarefied, scraping, shape = dataset), size=2.5, alpha=0.7, col=cols[2])  +
    labs(y = expression(paste('m'^2,' ha'^-1, 'min'^-1)),
     x = 'Species richness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
      plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
          legend.position = 'none') +
    geom_point(data=rich.scrape.res, size=2,  col=cols[2],aes(x, visregRes, shape=dataset)) +th


g4<-ggplot(even.scrape.fit) + 
    geom_ribbon(aes(x, visregFit, ymin = visregLwr, ymax = visregUpr), alpha=0.1,fill=cols[2]) + 
    geom_line(aes(x, visregFit), col=cols[2]) +
    # geom_point(data=h, aes(site.rarefied, scraping, shape = dataset), size=2.5, alpha=0.7, col=cols[2])  +
    labs(y = '',
     x = 'Species evenness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
      plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
          legend.position = 'none') +
    geom_point(data=even.scrape.res, size=2,  col=cols[2],aes(x, visregRes, shape=dataset)) +th

g5<-ggplot(beta.scrape.fit) + 
    geom_ribbon(aes(x, visregFit, ymin = visregLwr, ymax = visregUpr), alpha=0.1,fill=cols[2]) + 
    geom_line(aes(x, visregFit), col=cols[2]) +
    labs(y = '',
     x = expression(paste(beta,'-diversity'))) +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none',
          # legend.spacing.x = unit(0, 'cm'),
          # legend.spacing.y = unit(0, 'cm'),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = unit(c(0, 0.25, 0.25, 0.25), "cm"),
          # legend.box.background = element_rect(colour = "black"),
           legend.text=element_text(size=9)) +
    guides(size = F, shape=guide_legend(nrow=4,byrow=TRUE,override.aes = list(col='black', size=2))) +
    geom_point(data=beta.scrape.res, size=2,  col=cols[2],aes(x, visregRes, shape=dataset)) +th



pdf(file='figures/Figure5_diversity.pdf', height = 5, width=8)
# plot_grid(g0, g1, g2, g3, g4, g5, labels=c('A', 'B', 'C', 'D' ,'E', 'F'), nrow=2)
plot_grid(g0, g2, g3, g5, labels=c('A', 'B', 'C', 'D'), nrow=2)
dev.off()

