setwd("~/Box Sync/PhD Feedbacks/Git Repository/grazing-gradients")

#grab Maldives subset data from 'herb' dataset
load('data/wio_gbr_herb_master.Rdata')
str(herb) ## check structure of data frame
unique(herb$dataset) ## check regions in herb
maldives <- herb[herb$dataset == 'Maldives',]
write.csv(maldives, file = 'data/maldives_master.csv')

library(ggplot2)
library(readr)
maldives_master <- read_csv("data/maldives_master.csv")
maldives <- maldives_master

str(maldives)
unique(maldives$FG) #3 funtional groups
unique(maldives$family) #7 families
unique(maldives$species) #30 species
unique(maldives$habitat) #only 1 level
unique(maldives$management) #only 1 level
unique(maldives$depth) #only 1 level
unique(maldives$site) #11 sites
unique(maldives$transect.area) #250 & 100 m^2 
unique(maldives$abundance.500m2) #abundances 2 & 5
aggregate(species ~ FG, maldives, function(x) length(unique(x))) #3 browsers, 13 grazers, 14 scrapers
barplot(prop.table(table(maldives$FG))) #Grazer dominated, then scrapers, few browsers

#biomass gradient
maldives$logbiomass <- log10(maldives$biomass.kgha + 1)
par(mfrow = c(2, 3))
dotchart(maldives$biomass.kgha[maldives$FG=="Herbivore Grazer"], main = "Grazers", 
         ylab = "Order of observations", xlab = "Biomass kg/ha")
#min 2 outliers
dotchart(maldives$biomass.kgha[maldives$FG=="Herbivore Scraper"],main = "Scrapers",
         ylab = "Order of observations", xlab = "Biomass kg/ha") 
#min 1 outlier
dotchart(maldives$biomass.kgha[maldives$FG=="Herbivore Browser"], main = "Browsers",
         ylab = "Order of observations", xlab = "Biomass kg/ha")
#no outliers
hist(maldives$logbiomass[maldives$FG=="Herbivore Grazer"], main = "Grazers", xlab = "Log Biomass kg/ha")
hist(maldives$logbiomass[maldives$FG=="Herbivore Scraper"], main = "Scrapers", xlab = "Log Biomass kg/ha")
hist(maldives$logbiomass[maldives$FG=="Herbivore Browser"], main = "Browsers", xlab = "Log Biomass kg/ha")

ggplot(maldives, aes(log(mass.g, 2), fill=FG)) + geom_histogram(breaks=c(1:10)) + 
  facet_wrap(~FG) + theme(legend.position='none') + scale_x_continuous(breaks=c(1:10), 
  labels=c(2^c(1:10))) + labs (x='Mass (g)')
#Browsers few but large, grazers smaller but many, scrapers more equally spread in both


#common species, number species per transect & site
par(mfrow = c(1,1), mar = c(9, 4, 2, 2))
barplot(prop.table(table(maldives$species)), las = 2, cex.names = 0.7, font = 3)
par(mfrow = c(1,1), mar = c(11, 4, 2, 2))
boxplot(maldives$biomass.kgha ~ maldives$species, las = 2, cex.names = 0.6)

table(maldives$site, maldives$transect)
par(mfrow = c(1,1), mar = c(5, 4, 2, 2))
barplot(prop.table(table(maldives$site)), las = 2, cex.names = 0.7)
boxplot(maldives$biomass.kgha ~ maldives$site, las = 2, cex.names = 0.7)


#site, biomass, species only variables of interest

biom <- maldives %>%
  group_by(site, transect, FG) %>% 
  summarise(biom = sum(biomass.kgha)) %>%
  group_by(site, FG) %>%
  summarise(biom = mean (biom))

ggplot(biom, aes(site, log10(biom+1))) + geom_boxplot() + facet_wrap(~FG)
