setwd("C:/Users/jenee/Documents/git_repos/grazing-gradients")
load("data/wio_gbr_herb_master.Rdata")
str(herb) ## check structure of data frame
unique(herb$dataset) ## check regions in herb
chagos <- herb[herb$dataset == 'Chagos',]
write.csv(chagos, file = 'data/chagos_master.csv')

install.packages("ggplot2")
library(ggplot2)

chagos$FG <- as.factor(chagos$FG)
levels(chagos$FG)

#log scale biomass gradient
chagos$logbiomass <- log10(chagos$biomass.kgha)
ggplot(chagos, aes(x=logbiomass, fill=FG)) + geom_density(alpha=.3)
