setwd("~/Documents/git_repos/grazing-gradients")
load("data/wio_herb_benthic_merged.Rdata")

library(ggplot2)

#looking at biomass of FG along coral cover gradient
# Change point shapes and colors = U-G-L-Y
pred$logbiom <- log(pred$biomass.kgha)
ggplot(pred, aes(x=hard.coral, y=logbiom, shape=FG, color=FG)) +
  geom_point()

#split pannels with points
ggplot(pred, aes(hard.coral, biomass.kgha)) +
  geom_point() +
  geom_smooth(span = 0.8) +
  facet_wrap(~FG)

#split pannels without points
ggplot(pred, aes(hard.coral, biomass.kgha)) +
  geom_smooth(span = 0.8) +
  facet_wrap(~FG)

#FG across macroalgae gradient
ggplot(pred, aes(macroalgae, logbiom)) +
  #geom_point() +
  geom_smooth(span = 0.8) +
  facet_wrap(~FG)

#FG across complexity gradient
ggplot(pred, aes(complexity, logbiom)) +
  #geom_point() +
  geom_smooth(span = 0.8) +
  facet_wrap(~FG)

