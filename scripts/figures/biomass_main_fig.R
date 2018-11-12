
library(here)
setwd(here('grazing-gradients'))

pdf(file = "figures/figure2_panels.pdf", width=15, height=4)

## load models and predictions

load("results/models/biomass_m.browsers.Rdata")
load("results/models/biomass_m.scrapers.Rdata")
load("results/models/biomass_m.grazers.Rdata")
load("results/models/biomass_m.predictions.Rdata")


require(gridExtra)
library(grid)
library(lme4)
library(sjPlot)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)


## organize data to make 1st 2 panels for macroalgae and substrate#########

## James - need to setup dataframe h here! I copied these lines straight from 01_biomass.Rmd
load("data/wio_herb_benthic_merged.Rdata")

## assign seychelles 2017 with mean complexity values for now - needs fixed
pred$complexity[pred$dataset == 'Seychelles' & pred$date == 2017] <- mean(pred$complexity)

# estimate mean biomass per site per FG
h <- pred %>% 
  ## sum biomass per FG in each transect
  group_by(dataset, date, reef, site, management, transect, 
           unique.id, depth, FG,
           hard.coral, macroalgae, rubble, substrate, complexity, fish.biom) %>%
  summarise(biom = sum(biomass.kgha)) %>%
  ## mean FG biomass across transects at each site
  group_by(dataset, date, reef, site, management, unique.id, depth, FG,
           hard.coral, macroalgae,  rubble, substrate, complexity, fish.biom) %>%
  summarise(biom = mean(biom)) 


h<- spread(h, FG, biom, fill=0)
colnames(h)[14:16]<-c('browser', 'grazer', 'scraper')
h<-as.data.frame(h)

## scale vars to keep covariate means = 0. This is helpful for comparing effect sizes when covariates are on different scales.
h$hard.coral <- scale(h$hard.coral)
h$macroalgae <- scale(h$macroalgae)
h$complexity <- scale(h$complexity)
h$rubble <- scale(h$rubble)
h$substrate <- scale(h$substrate)
h$fish.biom <- scale(h$fish.biom)

## make dummy variables
h$fish.dummy<-ifelse(h$management=='Fished', 1, 0)
h$pristine.dummy<-ifelse(h$management=='Unfished', 1, 0)

## convert biomass to log10
h$grazerlog10<-log10(h$grazer+1)
h$scraperlog10<-log10(h$scraper +1)
h$browserlog10<-log10(h$browser+1)

## ------------------------------------------------ ##
      ## Now model predictions for covariates of interest ##
## ------------------------------------------------ ##

#####################################################################
#GRAZERS
cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)

#macroalgae
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$hard.coral <- 0; nd.g$complexity <-0; nd.g$rubble <- 0; nd.g$substrate <- 0
p.algae.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)

## substrate
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$macroalgae <- 0; nd.g$hard.coral <-0; nd.g$rubble <- 0; nd.g$complexity <- 0
p.substrate.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)

#fishable biomass


#####################################################################
#BROWSERS

cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)

#Macroalgae
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$hard.coral <- 0; nd.b$complexity <-0; nd.b$rubble <- 0; nd.b$substrate <- 0
p.algae.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)


#substrate
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$macroalgae <- 0; nd.b$hard.coral <-0; nd.b$rubble <- 0; nd.b$complexity <- 0
p.substrate.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)


############################################################################
#SCRAPERS
cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)


## Macroalgae
nd.s<-cont.pred.master
## set non-focal benthic covariates to 0
nd.s$hard.coral <- 0; nd.s$complexity <-0; nd.s$rubble <- 0; nd.s$substrate <- 0
p.algae.s<-predict(object=m.scraper, newdata = nd.s, re.form=NA)



## Substrate
nd.s<-cont.pred.master
## set non-focal benthic covariates to 0
nd.s$macroalgae <- 0; nd.s$hard.coral <-0; nd.s$rubble <- 0; nd.s$complexity <- 0
p.substrate.s<-predict(object=m.scraper, newdata = nd.s, re.form=NA)




## ------------------------------------------------ ##
  #### Make 2 panels for macroalgae and substrate
## ------------------------------------------------ ##

#Panel 2: macroalgae
p.algae.g <- as.data.frame(p.algae.g)
p.algae.g$model <- "grazers"
colnames(p.algae.g)[1] <- "y"

p.algae.b <- as.data.frame(p.algae.b)
p.algae.b$model <- "browsers"
colnames(p.algae.b)[1] <- "y"


p.algae.s <- as.data.frame(p.algae.s)
p.algae.s$model <- "scrapers"
colnames(p.algae.s)[1] <- "y"


p.algae <- rbind(p.algae.g, p.algae.b, p.algae.s)
p.algae <- cbind(p.algae, cont.pred.master$macroalgae)
colnames(p.algae)[3] <- "x"


#Panel 3: available substrate
p.substrate.g <- as.data.frame(p.substrate.g)
p.substrate.g$model <- "grazers"
colnames(p.substrate.g)[1] <- "y"

p.substrate.b <- as.data.frame(p.substrate.b)
p.substrate.b$model <- "browsers"
colnames(p.substrate.b)[1] <- "y"


p.substrate.s <- as.data.frame(p.substrate.s)
p.substrate.s$model <- "scrapers"
colnames(p.substrate.s)[1] <- "y"


p.substrate <- rbind(p.substrate.g, p.substrate.b, p.substrate.s)
p.substrate <- cbind(p.substrate, cont.pred.master$substrate)
colnames(p.substrate)[3] <- "x"


## ------------------------------------------------ ##
## arrange fishable biomass data for last panel 4: fishable biomass ##
## ------------------------------------------------ ##

# we need a new prediction data frame for the fishing effects, which are categorical.
## Let's use expand.grid to get all combinations of fishing variables, holding benthic covariates to 0
cat.pred.master<-expand.grid(hard.coral = 0,
                             macroalgae = 0,
                             rubble = 0, substrate = 0,
                             complexity = 0, fish.biom = 0, fish.dummy = c(0,1), pristine.dummy=c(0,1))

## Wait. this is wrong. We don't have fished AND pristine sites (duh). Let's drop that row.
cat.pred.master<-cat.pred.master[-4,]

## get fish biom range
fish.master<-data.frame(hard.coral = 0, macroalgae = 0, rubble = 0, complexity = 0, substrate = 0, fish.dummy = 0, pristine.dummy = 0, fish.biom = seq(min(h$fish.biom), max(h$fish.biom), length.out = 30))
fish.master.g<-data.frame(hard.coral = 0, macroalgae = 0, rubble = 0, complexity = 0, substrate = 0, fish.dummy = 0, pristine.dummy = 0, fish.biom = seq(min(h$fish.biom), max(h$fish.biom), length.out = 30))
fish.master.b<-data.frame(hard.coral = 0, macroalgae = 0, rubble = 0, complexity = 0, substrate = 0, fish.dummy = 0, pristine.dummy = 0, fish.biom = seq(min(h$fish.biom), max(h$fish.biom), length.out = 30))
fish.master.s<-data.frame(hard.coral = 0, macroalgae = 0, rubble = 0, complexity = 0, substrate = 0, fish.dummy = 0, pristine.dummy = 0, fish.biom = seq(min(h$fish.biom), max(h$fish.biom), length.out = 30))



##predict grazer biomass for fishing biomass gradient
fish.master.g$p.fish<-predict(object=m.grazer, newdata = fish.master.g, re.form=NA)
fish.master.b$p.fish<-predict(object=m.browser, newdata = fish.master.b, re.form=NA)
fish.master.s$p.fish<-predict(object=m.scraper, newdata = fish.master.s, re.form=NA)


fish.master.g$model <- "grazers"
fish.master.b$model <- "browsers"
fish.master.s$model <- "scrapers"

fish.master <- rbind(fish.master.g, fish.master.b, fish.master.s)
colnames(fish.master)[8] <- "x"
colnames(fish.master)[9] <- "y"

## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##
          ## ------------ NOW PLOTTING FIGURES -------------- ## 
## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##

## setup formatting information

linewidth = 4
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('grazers' = pal[5], 'scrapers' = pal[12], 'browsers' = pal[18])
theme_set(theme_sleek())

## axis units
ma.lab<-data.frame(labels = round(seq(min(pred$macroalgae), max(pred$macroalgae), length.out=5), 0),
                    breaks = seq(min(h$macroalgae), max(h$macroalgae), length.out=5))
substrate.lab<-data.frame(labels = round(seq(min(pred$substrate), max(pred$substrate), length.out=5), 0),
                    breaks = seq(min(h$substrate), max(h$substrate), length.out=5))
fishable.lab<-data.frame(labels = round(seq(min(pred$fish.biom), max(pred$fish.biom), length.out=5), 0),
                    breaks = seq(min(h$fish.biom), max(h$fish.biom), length.out=5))


#Panel 1: relative effect sizes of each FG (merged pic )

labs<-data.frame(lab=c('Hard coral', 'Macroalgae', 'Rubble', 'Available substrate', 'Complexity', 'Fishable biomass', 'Fished',   'Pristine'),
                 model=c('hard.coral', 'macroalgae', 'rubble', 'substrate','complexity', 'fish.biom', 'fish.dummy', 'pristine.dummy'))

## extract correct order of names according to increasing parameter estimate size
od<-sort(fixef(m.scraper), decreasing=T)
od<-od[-which(names(od) == '(Intercept)')]

## careful here for legend: colours defined by order of models, but other panels is defined by alphabetical order
g1 <- plot_models(m.grazer, m.scraper, m.browser, legend.title = "", 
                  m.labels=c("Grazer", "Scraper", "Browser"), 
                  colors=cols) +
                  #axis.labels = rev(labs$lab[match(names(od), labs$model)])) +
          theme(legend.position=c(0.7, 0.4)) 


g2 <- ggplot(p.algae, aes(x, y,  color = factor(model))) + 
        geom_line(size=linewidth) + 
        labs(title = "") +
        scale_color_manual(values = cols.named) +
        scale_x_continuous(breaks = ma.lab$breaks, labels = ma.lab$labels) +
        guides(col=F) +
        xlab("Macroalgae (%)") + ylab("Log10 Biomass (kg/ha)") 



g3 <- ggplot(p.substrate, aes(x, y, group=model, color = model)) + 
        geom_line(size=linewidth) + 
        labs(title = "") +
        scale_color_manual(values = cols.named) +
        guides(col=F) +
        scale_x_continuous(breaks = substrate.lab$breaks, labels = substrate.lab$labels) +
        xlab("Available substrate (%)") + ylab("")


g4 <- ggplot(fish.master, aes(x, y, group=model, color = model)) + 
        geom_line(size=linewidth) + 
        labs(title = "") +
        scale_color_manual(values = cols.named) +
        scale_x_continuous(breaks = fishable.lab$breaks, labels = comma(fishable.lab$labels)) +
        guides(col=F) +
        xlab("Fishable biomass (kg/ha)") + ylab("")

## ------------------------------------------------ ##
#### plot all the models on one graph 
#bring in all figs in 1 pannel

#grid.arrange(g1, g2, g3, g4)

##plot grid has easy labelling options
plot_grid(g2, g3, g4, g1, nrow =1, labels=c('a', 'b', 'c', 'd'))

dev.off()

## ------------------------------------------------ ##

# END

