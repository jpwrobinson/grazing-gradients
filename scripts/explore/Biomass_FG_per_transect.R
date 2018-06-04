#Find the mean biomass of each FG in each country-transect
#Across all sampling years and for different lengthed transects (!)
library(dplyr)
meanbiomass <- herb %>%
  group_by(dataset, site.number, transect, FG) %>%
  summarise_at(vars(biomass.kgha), funs(mean(., na.rm=TRUE)))
