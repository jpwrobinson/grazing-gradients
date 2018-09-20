
## Script to load Hoey bite rate datasets

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(tidyverse); library(funk)

## get UVC fish list
sp<-read.csv('data/herbivore_species_list.csv')
sp$genus<-str_split_fixed(sp$species, ' ', 2)[,1]
sp$sp<-str_split_fixed(sp$species, ' ', 2)[,2]

# ------------------------------------ #
	### CLEANING RED SEA DATASET ###
# ------------------------------------ #
redsea<-read.csv('data/bite-rates/raw/hoey_redsea.csv')
redsea$dataset<-'redsea'

## add dummy Reef variable
redsea<-cbind(Region = redsea[,1], Reef = rep(NA,dim(redsea)[1]), redsea[,2:11])

# ------------------------------------ #
	### CLEANING INDONESIA DATASET ###
# ------------------------------------ #
indonesia<-read.csv('data/bite-rates/raw/hoey_indonesia.csv')
indonesia$dataset<-'indonesia'
indonesia$Water.temp<-NULL
colnames(indonesia)[10]<-'Bite.rate'

## cut initialled genus info
indonesia$Species<-str_split_fixed(indonesia$Species, '. ', 2)[,2]

## add genus info from UVC list
indonesia$Genus<-sp$genus[match(indonesia$Species, sp$sp)]
## place Genus into 2nd column poisition
indonesia<-indonesia[,c(1,2,12,3:11)]



# ------------------------------------ #
	### CLEANING GBR DATASET ###
# ------------------------------------ #
gbr<-read.csv('data/bite-rates/raw/hoey_gbr.csv')
gbr$dataset<-'gbr'
gbr$TL.1<-NULL

## cut initialled genus info
gbr$Species<-str_split_fixed(gbr$Species, '. ', 2)[,2]

## add genus info from UVC list
gbr$Genus<-sp$genus[match(gbr$Species, sp$sp)]
## place Genus into 2nd column poisition
gbr<-gbr[,c(1,2,12,3:11)]

colnames(gbr)[11]<-'Bite.rate'

## merge datasets
bite<-rbind(redsea, indonesia, gbr)

## add species column
bite$sp<-with(bite, paste(Genus, Species))
## in UVC data?
bite$UVC<-ifelse(bite$sp %in% sp$species, 'TRUE', 'FALSE')

uniques(bite$sp[bite$UVC == TRUE]) ## 30 species
uniques(bite$sp[bite$UVC == FALSE]) ## 8 species

# > unique(bite$sp[bite$UVC == FALSE]) ## 8 species
# [1] "Chlorurus genozonatus" "Chlorurus gibbus"      "Scarus ferrugenius"   
# [4] "Scarus fuscopurpureus" "NA quoyi"              "NA russelli"          
# [7] "NA "                   "NA muricatum"   

## double check missing species are GONE
sp[grepl('genoz', sp$sp),]
sp[grepl('gib', sp$sp),]
sp[grepl('ferr', sp$sp),]
sp[grepl('fusco', sp$sp),]
sp[grepl('quoy', sp$sp),]
sp[grepl('russ', sp$sp),]
sp[grepl('muri', sp$sp),]



## checked. now save bite, filtered for UVC species
bite<-bite %>% filter(UVC == TRUE)
write.csv(bite, file = 'data/bite-rates/bite_rate_clean.csv')
