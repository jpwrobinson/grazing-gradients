

pdf(file = "figure 2. 4 panels.pdf", 10, 7)

## load models and predictions

load("~/git_repos/grazing-gradients/results/models/biomass_m.browsers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.scrapers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.grazers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.predictions.Rdata")


require(gridExtra)
library(grid)



#####################################################################
#GRAZERS
cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)

#algae
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$hard.coral <- 0; nd.g$complexity <-0; nd.g$rubble <- 0; nd.g$substrate <- 0
p.algae.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)

## substrate
nd.g<-cont.pred.master
## set non-focal benthic covariates to 0
nd.g$macroalgae <- 0; nd.g$hard.coral <-0; nd.g$rubble <- 0; nd.g$complexity <- 0
p.substrate.g<-predict(object=m.grazer, newdata = nd.g, re.form=NA)


#####################################################################
#BROWSERS

cont.pred.master<-data.frame(hard.coral = seq(min(h$hard.coral), max(h$hard.coral), length.out=30),
                             macroalgae = seq(min(h$macroalgae), max(h$macroalgae), length.out=30),
                             complexity = seq(min(h$complexity), max(h$complexity), length.out=30),
                             rubble = seq(min(h$rubble), max(h$rubble), length.out=30),
                             substrate = seq(min(h$substrate), max(h$substrate), length.out=30),
                             fish.biom = 0,
                             fish.dummy = 0, pristine.dummy=0)

## 2. predict browser biomass for macroalgae effect, holding every other covariate constant
nd.b<-cont.pred.master
## set non-focal benthic covariates to 0
nd.b$hard.coral <- 0; nd$complexity <-0; nd$rubble <- 0; nd$substrate <- 0
p.algae.b<-predict(object=m.browser, newdata = nd.b, re.form=NA)


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
#### FIGURE - 4 PANELS


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
 xlab("Benthic cover (%)") + ylab("Log10 Biomass (kg/ha")
g2



#Panel 3: available substrate
p.algae.g <- as.data.frame(p.substrate.g)
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
  xlab("Benthic cover (%)") + ylab("Log10 Biomass (kg/ha")
g2



#Panel 4: fishable biomass

