
## load models and predictions

load("~/git_repos/grazing-gradients/results/models/biomass_m.browsers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.scrapers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.grazers.Rdata")
load("~/git_repos/grazing-gradients/results/models/biomass_m.predictions.Rdata")


require(gridExtra)
library(grid)

#Panel 1: relative effect sizes of each FG (merged pic )
library(sjlabelled)


## create df for labelling plots with better names
labs<-data.frame(lab=c('Hard coral', 'Macroalgae', 'Rubble', 'Available substrate', 'Complexity', 'Fishable biomass', 'Fished',   'Pristine'),
                 model=c('hard.coral', 'macroalgae', 'rubble', 'substrate','complexity', 'fish.biom', 'fish.dummy', 'pristine.dummy'))

## extract correct order of names according to increasing parameter estimate size
od<-sort(fixef(m.grazer), decreasing=T)
od<-od[-which(names(od) == '(Intercept)')]



#set up the viewframe panel
par(mfrow=c(2,2), mar=c(4,4,1,1), oma=c(0,0,0,0))
vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), 
                           just=c("left","top"), 
                           y=0.5, x=0.5)


g4 <- plot_models(m.scraper, m.grazer, m.browser, legend.title = "Functional group", 
                  axis.labels = rev(labs$lab[match(names(od), labs$model)])) 




#Panel 2: macroalgae
plot(cont.pred.master$macroalgae, p.algae, type='l',col = cols[2],lwd=2, axes=T, ylab='Log10 Biomass (kg/ha)', xlab='Benthic cover (%)', ylim=c(2, 3))
axis(1, at = seq(min(h$macroalgae), max(h$macroalgae), length.out=5), 
     labels= round(seq(min(pred$macroalgae), max(pred$macroalgae), length.out=5), 0))
axis(2)
add_label(0.01, 0.1, font = 2, label ='Macroalgae')
mtext(2, text= 'Biomass (log10 kg/ha)', outer=TRUE, line=0.5)


#Panel 3: available substrate
plot(cont.pred.master$substrate, p.substrate, type='l',col = cols[2],lwd=2, axes=T, ylab='Log10 Biomass (kg/ha)', xlab='Available substrate', ylim=c(2, 3))
axis(1, at = seq(min(h$substrate), max(h$substrate), length.out=5), 
     labels= round(seq(min(pred$substrate), max(pred$substrate), length.out=5), 0))
axis(2)
add_label(0.01, 0.1, font = 2, label ='Substrate')
mtext(2, text= 'Biomass (log10 kg/ha)', outer=TRUE, line=0.5)


#Panel 4: fishable biomass

plot(fish.master$fish.biom, fish.master$p.fish, cex=2, axes=T, ylab='Log10 Biomass (kg/ha)', xlab='Fishable biomass (kg/ha)', ylim=c(2,3), type='l')
axis(1, at = seq(min(h$fish.biom), max(h$fish.biom), length.out=5), 
     labels= comma(round(seq(min(pred$fish.biom), max(pred$fish.biom), length.out=5), 0)))
axis(2)
add_label(0.01, 0.1, font = 2, label ='Fishable biomass')
mtext(2, text= 'Biomass (log10 kg/ha)', outer=TRUE, line=0.5)

print(g4, vp=vp.BottomRight)

