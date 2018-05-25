setwd("C:/Users/jenee/Documents/git_repos/grazing-gradients")
load("data/wio_gbr_herb_master.Rdata")
str(herb) ## check structure of data frame
unique(herb$dataset) ## check regions in herb
chagos <- herb[herb$dataset == 'Chagos',]
write.csv(chagos, file = 'data/chagos_master.csv')

#Load packages
library(tidyverse)
library (ggplot2)

#Get some descriptive info on survey data
summary(chagos) #5729 records
#Number of sites
chagos$site <- as.factor(chagos$site)
levels(chagos$site) #20 sites
#Number of reefs
chagos$reef <- as.factor(chagos$reef) #4 reefs
#Number of transects per site
chagos$transect <- as.factor(chagos$transect) #1-4 transects
chagos %>% count(site, transect, sort =TRUE)
#Number of sitings of each species
chagos$species <- as.factor(chagos$species)
chagos %>% count(species) # 52 species

#log scale biomass gradient
chagos$FG <- as.factor(chagos$FG)
levels(chagos$FG)
chagos$logbiomass <- log10(chagos$biomass.kgha)
ggplot(chagos, aes(x=logbiomass, fill=FG)) + geom_density(alpha=.3)


#Comparing sheltered and unsheltered functional groups
#Grouped bar chart showing biomass of each FG at different habitats, blank is unrecorded habitat type
ggplot(chagos, aes(fill=FG, y=biomass.kgha, x=habitat)) +
  geom_bar(position="dodge", stat="identity")


#Comaparing depth at 3m and 9m
str(chagos) #change depth from numeric to character for graph
chagos$depth <- as.character(chagos$depth)
#Plot biomass of each FG at 3 and 9 m 
ggplot(chagos, aes(fill=FG, y=biomass.kgha, x=depth)) +
  geom_bar(position="dodge", stat="identity")




