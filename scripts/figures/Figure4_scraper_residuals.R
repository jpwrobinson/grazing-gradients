
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


## get diversity
# data load
load("data/wio_herb_benthic_merged.Rdata")
# estimate mean biomass per site per FG
h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id, species) %>%
          summarise(biom = sum(biomass.kgha)) %>%
  ## mean species biomass across transects at each site
          group_by(unique.id, species) %>%
          summarise(biom = mean(biom)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, biom)
com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -c(1)]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format
com.mat<-as.matrix(com.mat)
dim(com.mat)


## estimate diversity
library(vegan)
div<-data.frame(div=diversity(com.mat), 
				richness=specnumber(com.mat), 
				unique.id = rows)
div$J <- div$div/log(div$richness)

# save mean sizes 
sizes<-pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(dataset, reef, site, transect, 
                 unique.id) %>%
          summarise(size = mean(length.cm), mass= mean(mass.g)) %>%
  ## mean species sizeass across transects at each site
          group_by(unique.id) %>%
          summarise(size = mean(size), mass=mean(mass)) 

div$mean.size<-sizes$size
div$mean.mass<-sizes$mass

## scraping models
load("results/models/scraping_model.Rdata")
rsquared(m.scraper)
# Marginal Conditional
#  0.1459465   0.6475017

## scraping data
load('results/models/scraper_function.Rdata')
m.scrape<-lmer(scraping ~ biom + (1 | dataset), h)
h$resid<-resid(m.scrape)

## attach to t
h$simpson.diversity<-div$div[match(h$unique.id, div$unique_id)] ## simpson is 1 - D. 
h$sp.richness<-div$richness[match(h$unique.id, div$unique_id)]
h$evenness<-div$J[match(h$unique.id, div$unique_id)]
h$mean.size<-div$mean.size[match(h$unique.id, div$unique_id)]
h$mean.mass<-div$mean.mass[match(h$unique.id, div$unique_id)]

## assign seychelles 2017 with mean complexity values for now - needs fixed
h$complexity[h$dataset == 'Seychelles' & h$date == 2017] <- mean(h$complexity)


h$simpson.diversity.scaled <- scale(h$simpson.diversity)
h$sp.richness.scaled <- scale(h$sp.richness)
h$evenness.scaled <- scale(h$evenness)
h$mean.size.scaled <- scale(h$mean.size)
h$mean.mass.scaled <- scale(h$mean.mass)
h$biom.scaled <- scale(h$biom)

## new decoupling model to account for div and size structure
m.scrape2<-lmer(scraping ~ biom.scaled + mean.mass.scaled + sp.richness.scaled + evenness.scaled +
          (1 | dataset/reef), h)

nd.rich<-data.frame(sp.richness.scaled = seq(min(h$sp.richness.scaled), max(h$sp.richness.scaled), length.out=20),
              sp.richness.raw = seq(min(h$sp.richness), max(h$sp.richness), length.out=20),
                          biom.scaled=0, evenness.scaled = 0, mean.mass.scaled = 0, dataset = 'GBR', reef='1')
nd.rich$pred<-predict(m.scrape2, newdata=nd.rich, re.form=NA)

nd.size<-data.frame(mean.mass.scaled = seq(min(h$mean.mass.scaled), max(h$mean.mass.scaled), length.out=20),
              mean.mass.raw = seq(min(h$mean.mass), max(h$mean.mass), length.out=20),
                          biom.scaled=0, evenness.scaled = 0, sp.richness.scaled = 0, dataset = 'GBR', reef='1')
nd.size$pred<-predict(m.scrape2, newdata=nd.size, re.form=NA)


partials<-visreg::visreg(m.scrape2, 'sp.richness.scaled')
richness.fit<-data.frame(fit=partials$fit$visregFit, x = partials$fit$sp.richness, 
  richness = seq(min(h$sp.richness), max(h$sp.richness), length.out = 101))

richness.points<-data.frame(x=h$sp.richness, y = partials$res$visregRes, dataset=partials$res$dataset)

partials<-visreg::visreg(m.scrape2, 'mean.mass.scaled')
mean.mass.fit<-data.frame(fit=partials$fit$visregFit, x = partials$fit$mean.mass, 
  mean.mass = seq(min(h$mean.mass), max(h$mean.mass), length.out = 101))

mean.mass.points<-data.frame(x=h$mean.mass, y = partials$res$visregRes, dataset=partials$res$dataset)

str(partials)
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[12])

g1<-ggplot(richness.fit, aes(richness, fit)) + geom_line() + 
    geom_point(data=richness.points, aes(x, y), alpha=0.7, col=cols)  +
    labs(y = 'Partial effect on area scraped', x = 'Species richness') +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = c(0.8, 0.9))


g2<-ggplot(mean.mass.fit, aes(mean.mass, fit)) + geom_line() + 
    geom_point(data=mean.mass.points, aes(x, y), alpha=0.7, col=cols)  +
    labs(y = '', x = 'Mean size (g)') +
    scale_x_continuous(labels=comma) +
    # geom_hline(yintercept=0, linetype=5, col='grey') +
    theme(legend.title=element_blank(),
          legend.position = 'none')


pdf(file='figures/Figure4_scraper_resids.pdf', height = 4, width=9)
plot_grid(g1, g2, labels=c('a', 'b'))
dev.off()


