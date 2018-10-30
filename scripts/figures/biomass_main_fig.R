

pdf(file = "figures/final/figure2_panels.pdf", 10, 7)

## load models and predictions

load("~/git_repos/grazing-gradients/results/models/biomass_m.browsers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.scrapers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.grazers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.predictions.Rdata")


require(gridExtra)
library(grid)
library(lme4)
library(sjPlot)
library(tidyverse)

## organize data to make 1st 2 panels for macroalgae and substrate#########
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
nd.b$hard.coral <- 0; nd$complexity <-0; nd$rubble <- 0; nd$substrate <- 0
p.algae.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)


#substrate
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$macroalgae <- 0; nd$hard.coral <-0; nd$rubble <- 0; nd$complexity <- 0
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



#########################################################################
##########################################################
################################
#### Make effect size panel and next 2 panels for macroalgae and substrate


#Panel 1: relative effect sizes of each FG (merged pic )
g1 <- plot_models(m.scraper, m.grazer, m.browser, legend.title = "Functional group", 
                  axis.labels = rev(labs$lab[match(names(od), labs$model)])) 


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

library(ggplot2)
g2 <- ggplot(p.algae, aes(x, y, group=model, color = model)) + geom_point() + geom_line() + labs(title = "Macroalgae") +
 xlab("Benthic cover (%)") + ylab("Log10 Biomass (kg/ha)")
g2



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


g3 <- ggplot(p.substrate, aes(x, y, group=model, color = model)) + geom_point() + geom_line() + labs(title = "Substrate") +
  xlab("Available substrate") + ylab("Log10 Biomass (kg/ha)")
g3


###############################
############### arrange fishable biomass data for last panel
#Panel 4: fishable biomass

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


fish.master.g$model <- "grazer"
fish.master.b$model <- "browser"
fish.master.s$model <- "scraper"

fish.master <- rbind(fish.master.b, fish.master.g, fish.master.s)
colnames(fish.master)[8] <- "x"
colnames(fish.master)[9] <- "y"

#### plot all the models on one graph 

g4 <- ggplot(fish.master, aes(x, y, group=model, color = model)) + geom_point() + geom_line() + labs(title = "Fishable biomass") +
  xlab("Fishable biomass (kg/ha)") + ylab("Log10 Biomass (kg/ha)")
g4



###############
#bring in all figs in 1 pannel

grid.arrange(g1, g2, g3, g4)
