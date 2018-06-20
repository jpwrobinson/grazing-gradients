#Load packages
library(dbplyr)
library(tidyr)
library(plyr)


#Load full species list
herbivores <- read.csv("data/herbivore_species_list.csv")


#Load functional groups of each region
FG_all <- read.csv("data/FG_checking_across_sites.csv")
FG_chagos <- select(FG_all, "ChagosSP", "ChagosFG")
FG_GBR <- select(FG_all, "GBRSP", "GBRFG")
FG_maldives <- select(FG_all, "MaldivesSP", "MaldivesFG")
FG_seychelles <- select(FG_all, "SeychellesSP", "SeychellesFG")


#Delete duplicates
FG_chagos <- FG_chagos %>% distinct(ChagosSP, .keep_all = TRUE)
FG_GBR <- FG_GBR %>% distinct(GBRSP, .keep_all = TRUE)
FG_maldives <- FG_maldives %>% distinct(MaldivesSP, .keep_all = TRUE)
FG_seychelles <- FG_seychelles %>% distinct(SeychellesSP, .keep_all = TRUE)

#Add region to each line
FG_chagos$site <- "Chagos"
FG_GBR$site <- "GBR"
FG_maldives$site <- "Maldives"
FG_seychelles$site <- "Seychelles"

#compare FG across regions for each species
all <- merge(herbivores, FG_chagos, by.x = "species", by.y = "ChagosSP", all = TRUE)
all <-  merge(all, FG_GBR, by.x = "species", by.y = "GBRSP", all = TRUE)
all <- merge(all, FG_maldives, by.x = "species", by.y = "MaldivesSP", all = TRUE)
all <- merge(all, FG_seychelles, by.x = "species", by.y = "SeychellesSP", all = TRUE)
all <- select(all, c("species", "ChagosFG","GBRFG", "MaldivesFG", "SeychellesFG"))


# Only issue is with Siganus argenteus => change to browser in GBR