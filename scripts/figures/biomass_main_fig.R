
library(here)
setwd(here('grazing-gradients'))

## load models and predictions

load("results/models/biomass_m.browsers.Rdata")
load("results/models/biomass_m.scrapers.Rdata")
load("results/models/biomass_m.grazers.Rdata")
load("results/models/biomass_m.predictions.Rdata")


require(gridExtra)
library(grid)
library(lme4)
#library(sjPlot)
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

## get mean reef values for overlaying figure points
h.means<-h %>% group_by(dataset, reef) %>% 
  summarise(browser=mean(browserlog10), grazer=mean(grazerlog10), scraper=mean(scraperlog10), hard.coral=mean(hard.coral),
            macroalgae=mean(macroalgae), rubble=mean(rubble), 
            substrate=mean(substrate), complexity=mean(complexity), fish.biom=mean(fish.biom)) %>%
  gather(FG, biom, -dataset, -reef, -hard.coral, -macroalgae, -rubble, -substrate, -complexity, -fish.biom) %>%
  mutate(FG = fct_recode(FG, "grazers" = "grazer", 'browsers' = 'browser', 'scrapers'='scraper'))


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

## coral
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$macroalgae <- 0; nd.g$complexity <-0; nd.g$rubble <- 0; nd.g$substrate <- 0
p.coral.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)

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

## Complexity
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$macroalgae <- 0; nd.g$hard.coral <-0; nd.g$rubble <- 0; nd.g$substrate <- 0
p.complexity.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)

## Rubble
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$macroalgae <- 0; nd.g$hard.coral <-0; nd.g$complexity <- 0; nd.g$substrate <- 0
p.rubble.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)


#####################################################################
#BROWSERS

cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)

## coral
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$macroalgae <- 0; nd.b$complexity <-0; nd.b$rubble <- 0; nd.b$substrate <- 0
p.coral.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)


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

## Complexity
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$macroalgae <- 0; nd.b$hard.coral <-0; nd.b$rubble <- 0; nd.b$substrate <- 0
p.complexity.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)

## Rubble
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$macroalgae <- 0; nd.b$hard.coral <-0; nd.b$complexity <- 0; nd.b$substrate <- 0
p.rubble.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)

############################################################################
#SCRAPERS
cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)


## coral
nd.s<-cont.pred.master
## set non-focal benthic covariates to 0
nd.s$macroalgae <- 0; nd.s$complexity <-0; nd.s$rubble <- 0; nd.s$substrate <- 0
p.coral.s<-predict(object=m.scraper, newdata = nd.s, re.form=NA)


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

## Complexity
nd.s<-cont.pred.master
## set non-focal benthic covariates to 0
nd.s$macroalgae <- 0; nd.s$hard.coral <-0; nd.s$rubble <- 0; nd.s$substrate <- 0
p.complexity.s<-predict(object=m.scraper, newdata = nd.s, re.form=NA)

## Rubble
nd.s<-cont.pred.master
## set non-focal benthic covariates to 0
nd.s$macroalgae <- 0; nd.s$hard.coral <-0; nd.s$complexity <- 0; nd.s$substrate <- 0
p.rubble.s<-predict(object=m.scraper, newdata = nd.s, re.form=NA)


## ------------------------------------------------ ##
  #### Make 2 panels for macroalgae and substrate
## ------------------------------------------------ ##


#Panel 1: coral
p.coral.g <- as.data.frame(p.coral.g)
p.coral.g$model <- "grazers"
colnames(p.coral.g)[1] <- "y"

p.coral.b <- as.data.frame(p.coral.b)
p.coral.b$model <- "browsers"
colnames(p.coral.b)[1] <- "y"


p.coral.s <- as.data.frame(p.coral.s)
p.coral.s$model <- "scrapers"
colnames(p.coral.s)[1] <- "y"


p.coral <- rbind(p.coral.g, p.coral.b, p.coral.s)
p.coral <- cbind(p.coral, cont.pred.master$hard.coral)
colnames(p.coral)[3] <- "x"



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

#Panel 4: rubble
p.rubble.g <- as.data.frame(p.rubble.g)
p.rubble.g$model <- "grazers"
colnames(p.rubble.g)[1] <- "y"

p.rubble.b <- as.data.frame(p.rubble.b)
p.rubble.b$model <- "browsers"
colnames(p.rubble.b)[1] <- "y"


p.rubble.s <- as.data.frame(p.rubble.s)
p.rubble.s$model <- "scrapers"
colnames(p.rubble.s)[1] <- "y"


p.rubble <- rbind(p.rubble.g, p.rubble.b, p.rubble.s)
p.rubble <- cbind(p.rubble, cont.pred.master$rubble)
colnames(p.rubble)[3] <- "x"


#Panel 5: complexity
p.complexity.g <- as.data.frame(p.complexity.g)
p.complexity.g$model <- "grazers"
colnames(p.complexity.g)[1] <- "y"

p.complexity.b <- as.data.frame(p.complexity.b)
p.complexity.b$model <- "browsers"
colnames(p.complexity.b)[1] <- "y"


p.complexity.s <- as.data.frame(p.complexity.s)
p.complexity.s$model <- "scrapers"
colnames(p.complexity.s)[1] <- "y"


p.complexity <- rbind(p.complexity.g, p.complexity.b, p.complexity.s)
p.complexity <- cbind(p.complexity, cont.pred.master$complexity)
colnames(p.complexity)[3] <- "x"

## ------------------------------------------------ ##
## arrange fishable biomass data for last panel 4: fishable biomass ##
## ------------------------------------------------ ##

## get fish biom range
fish.master<-data.frame(hard.coral = 0, macroalgae = 0, rubble = 0, complexity = 0, substrate = 0, fish.dummy = 0, pristine.dummy = 0, fish.biom = seq(min(h$fish.biom), max(h$fish.biom), length.out = 30))
fish.master.g<-fish.master
fish.master.b<-fish.master
fish.master.s<-fish.master



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


## ------------------------------------------------ ##
## arrange pristine and fishing effects estimates
## ------------------------------------------------ ##
# we need a new prediction data frame for the fishing effects, which are categorical.
# ## Let's use expand.grid to get all combinations of fishing variables, holding benthic covariates to 0
# cat.pred.master<-expand.grid(hard.coral = 0,
#                              macroalgae = 0,
#                              rubble = 0, substrate = 0,
#                              complexity = 0, fish.biom = 0, fish.dummy = c(0,1), pristine.dummy=c(0,1))

# ## Wait. this is wrong. We don't have fished AND pristine sites (duh). Let's drop that row.
# cat.pred.master<-cat.pred.master[-4,]

gr<-coef(summary(m.grazer))
rownames(gr)[rownames(gr)=='(Intercept)']<-'manage.dummy'
gr <- data.frame(gr[grepl('dummy', rownames(gr)),])
gr$model <- 'grazers'
gr$var<-rownames(gr)

## correct to estimated biomass
gr$Estimate[gr$var == 'fish.dummy']<-gr$Estimate[gr$var == 'fish.dummy'] + gr$Estimate[gr$var == 'manage.dummy']
gr$Estimate[gr$var == 'pristine.dummy']<-gr$Estimate[gr$var == 'pristine.dummy'] + gr$Estimate[gr$var == 'manage.dummy']


sc<-coef(summary(m.scraper))
rownames(sc)[rownames(sc)=='(Intercept)']<-'manage.dummy'
sc <- data.frame(sc[grepl('dummy', rownames(sc)),])
sc$model <- 'scrapers'
sc$var<-rownames(sc)

## correct to estimated biomass
sc$Estimate[sc$var == 'fish.dummy']<-sc$Estimate[sc$var == 'fish.dummy'] + sc$Estimate[sc$var == 'manage.dummy']
sc$Estimate[sc$var == 'pristine.dummy']<-sc$Estimate[sc$var == 'pristine.dummy'] + sc$Estimate[sc$var == 'manage.dummy']


br<-coef(summary(m.browser))
rownames(br)[rownames(br)=='(Intercept)']<-'manage.dummy'
br <- data.frame(br[grepl('dummy', rownames(br)),])
br$model <- 'browsers'
br$var<-rownames(br)

## correct to estimated biomass
br$Estimate[br$var == 'fish.dummy']<-br$Estimate[br$var == 'fish.dummy'] + br$Estimate[br$var == 'manage.dummy']
br$Estimate[br$var == 'pristine.dummy']<-br$Estimate[br$var == 'pristine.dummy'] + br$Estimate[br$var == 'manage.dummy']


eff<-rbind(gr, sc, br)
eff$upper<-with(eff, Estimate + 2*Std..Error)
eff$lower<-with(eff, Estimate - 2*Std..Error)

predict(m.grazer, newdata=)

## ------------------------------------------------ ##
## arrange benthic effects estimates
## ------------------------------------------------ ##
vars<-c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity')
gr<-coef(summary(m.grazer))
gr <- data.frame(gr[rownames(gr) %in% vars,])
gr$model <- 'grazers'
gr$var<-rownames(gr)

sc<-coef(summary(m.scraper))
sc <- data.frame(sc[rownames(sc) %in% vars,])
sc$model <- 'scrapers'
sc$var<-rownames(sc)

br<-coef(summary(m.browser))
br <- data.frame(br[rownames(br) %in% vars,])
br$model <- 'browsers'
br$var<-rownames(br)

eff.benthic<-rbind(gr, sc, br)
eff.benthic$upper<-with(eff.benthic, Estimate + 2*Std..Error)
eff.benthic$lower<-with(eff.benthic, Estimate - 2*Std..Error)


## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##
          ## ------------ NOW PLOTTING FIGURES -------------- ## 
## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##

## setup formatting information

linewidth = 2
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('grazers' = pal[5], 'scrapers' = pal[12], 'browsers' = pal[18])
theme_set(theme_sleek())

## axis units
coral.lab<-data.frame(labels = round(seq(min(pred$hard.coral), max(pred$hard.coral), length.out=5), 0),
                    breaks = seq(min(h$hard.coral), max(h$hard.coral), length.out=5))
ma.lab<-data.frame(labels = round(seq(min(pred$macroalgae), max(pred$macroalgae), length.out=5), 0),
                    breaks = seq(min(h$macroalgae), max(h$macroalgae), length.out=5))
substrate.lab<-data.frame(labels = round(seq(min(pred$substrate), max(pred$substrate), length.out=5), 0),
                    breaks = seq(min(h$substrate), max(h$substrate), length.out=5))
complexity.lab<-data.frame(labels = round(seq(min(pred$complexity), max(pred$complexity), length.out=5), 0),
                    breaks = seq(min(h$complexity), max(h$complexity), length.out=5))
rubble.lab<-data.frame(labels = round(seq(min(pred$rubble), max(pred$rubble), length.out=5), 0),
                    breaks = seq(min(h$rubble), max(h$rubble), length.out=5))
fishable.lab<-data.frame(labels = round(seq(min(pred$fish.biom), max(pred$fish.biom), length.out=5), 0),
                    breaks = seq(min(h$fish.biom), max(h$fish.biom), length.out=5))


#Panel 1: relative effect sizes of each FG (merged pic )

# labs<-data.frame(lab=c('Hard coral', 'Macroalgae', 'Rubble', 'Available substrate', 'Complexity', 'Fishable biomass', 'Fished',   'Pristine'),
#                  model=c('hard.coral', 'macroalgae', 'rubble', 'substrate','complexity', 'fish.biom', 'fish.dummy', 'pristine.dummy'))

## extract correct order of names according to increasing parameter estimate size
# od<-sort(fixef(m.scraper), decreasing=T)
# od<-od[-which(names(od) == '(Intercept)')]

## careful here for legend: colours defined by order of models, but other panels is defined by alphabetical order
g.effects <- ggplot(eff.benthic, aes(var, Estimate, col=model)) + 
              geom_hline(yintercept=0, linetype='dashed') +
              geom_pointrange(aes(ymin=lower, ymax=upper),size =1, position=position_dodge(width=0.4)) +
              scale_color_manual(values = cols.named) +
              # scale_x_discrete(labels = c('Fished', 'Protected', 'Pristine')) +
              labs(x='', y = 'Standardized effect size') +
              theme(legend.position = 'none') + coord_flip() +
              scale_x_discrete(position = "top")

eff$var<-factor(eff$var, levels=c('fish.dummy', 'manage.dummy', 'pristine.dummy'))
## careful here for legend: colours defined by order of models, but other panels is defined by alphabetical order
g.cats <- ggplot(eff, aes(var, Estimate, col=model)) + 
              # geom_hline(yintercept=0, linetype='dashed') +
              geom_pointrange(aes(ymin=lower, ymax=upper),size =1, position=position_dodge(width=0.4)) +
              scale_color_manual(values = cols.named) +
              scale_x_discrete(labels = c('Fished', 'Protected', 'Pristine')) +
              labs(x='', y = 'Predicted log10 biomass') +
              theme(legend.position = c(0.2,0.8), 
                      legend.title=element_blank())


g1 <- ggplot(p.coral, aes(x, 10^y,  color = factor(model))) + 
        geom_line(size=linewidth) + 
        #geom_point(data=h.means, aes(macroalgae, biom, color=FG), alpha=0.2, size=2) +
        labs(title = "") +
        scale_y_log10(labels=comma, limit=c(10, 500)) +
        scale_color_manual(values = cols.named) +
        scale_x_continuous(breaks = coral.lab$breaks, labels = coral.lab$labels) +
        guides(col=F) +
        xlab("Hard coral (%)") + ylab(expression(paste("biomass kg ", "ha"^-1))) 


g2 <- ggplot(p.algae, aes(x, 10^y,  color = factor(model))) + 
        geom_line(size=linewidth) + 
        #geom_point(data=h.means, aes(macroalgae, biom, color=FG), alpha=0.2, size=2) +
        labs(title = "") +
        scale_y_log10(labels=comma, limit=c(10, 500)) +
        scale_color_manual(values = cols.named) +
        scale_x_continuous(breaks = ma.lab$breaks, labels = ma.lab$labels) +
        guides(col=F) +
        xlab("Macroalgae (%)") + ylab('') 


g3 <- ggplot(p.substrate, aes(x, 10^y, color = model)) + 
        geom_line(size=linewidth) + 
        #geom_point(data=h.means, aes(substrate, biom, color=FG), alpha=0.2, size=2) +
        labs(title = "") +
        scale_y_log10(labels=comma, limit=c(10, 500)) +
        scale_color_manual(values = cols.named) +
        guides(col=F) +
        scale_x_continuous(breaks = substrate.lab$breaks, labels = substrate.lab$labels) +
        xlab("Available substrate (%)") + ylab("")

g4 <- ggplot(p.complexity, aes(x, 10^y, color = model)) + 
        geom_line(size=linewidth) + 
        #geom_point(data=h.means, aes(substrate, biom, color=FG), alpha=0.2, size=2) +
        labs(title = "") +
        scale_y_log10(labels=comma, limit=c(10, 500)) +
        scale_color_manual(values = cols.named) +
        guides(col=F) +
        scale_x_continuous(breaks = complexity.lab$breaks, labels = complexity.lab$labels) +
        xlab("Complexity") + ylab(expression(paste("biomass kg ", "ha"^-1))) 

g5 <- ggplot(p.rubble, aes(x, 10^y, color = model)) + 
        geom_line(size=linewidth) + 
        #geom_point(data=h.means, aes(rubble, biom, color=FG), alpha=0.2, size=2) +
        labs(title = "") +
        scale_y_log10(labels=comma, limit=c(10, 500)) +
        scale_color_manual(values = cols.named) +
        guides(col=F) +
        scale_x_continuous(breaks = rubble.lab$breaks, labels = rubble.lab$labels) +
        xlab("Rubble (%)") + ylab("")


g6 <- ggplot(fish.master, aes(x, 10^y, color = model)) + 
        geom_line(size=linewidth) + 
        #geom_point(data=h.means, aes(fish.biom, biom, color=FG), alpha=0.2, size=2) +
        labs(title = "") +
        scale_y_log10(labels=comma, limit=c(10, 1500)) +
        scale_color_manual(values = cols.named) +
        scale_x_continuous(breaks = fishable.lab$breaks, labels = comma(fishable.lab$labels)) +
        guides(col=F) +
        xlab(expression(paste("Fishable biomass (kg ", "ha"^-1,")"))) + ylab("")


## ------------------------------------------------ ##
#### plot all the models on one graph 
#bring in all figs in 1 pannel

#grid.arrange(g1, g2, g3, g4)

pdf(file = "figures/figure2_panels.pdf", width=12, height=8)


?plot_grid
top_side<-plot_grid(g1, g2, g3, g4, g5, g6, nrow =2, labels=c('a', 'b', 'c', 'd', 'e', 'f'), align='h')
bottom_side<-plot_grid(g.cats, g.effects, labels = c('g','h'), nrow=1, ncol=2, label_size=12)

plot_grid(top_side,bottom_side, ncol = 1, nrow=2, rel_heights=c(1, 1), label_size=12)



#plot_grid(g.cats, g.effects)

dev.off()

## ------------------------------------------------ ##

# END

