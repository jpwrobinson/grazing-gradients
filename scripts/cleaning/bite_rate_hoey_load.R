
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
indonesia <- indonesia[!is.na(indonesia$bite.rate..bites.min.),]
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

gbr$min[is.na(gbr$min)]<-gbr$sec[is.na(gbr$min)]/60


# ------------------------------------ #
	### CLEANING GRABA-LANDRY DATASET ###
# ------------------------------------ #
gra<-read.csv('data/bite-rates/raw/Graba-Landry_feeding_obs.csv')
gra <- gra[!is.na(gra$Total_Bites),]
## drop columns that can't be merged
gra$Date<-NULL
gra$Observer<-NULL
gra$Temperature<-NULL
gra$Grp_Size<-NULL
gra$Mixed_Grp<-NULL
gra$Start_Time<-NULL
gra$End_Time<-NULL
gra$minutes.60<-NULL
gra$add_to_.seconds<-NULL
gra$total_divided_by_60<-NULL
gra$Trial_Time<-NULL
gra$X<-NULL
gra$X.1<-NULL
## fix species names
gra$Species<-str_split_fixed(gra$Species, '_', 2)[,2]

## fix 2 species names
gra$Species[which(gra$Species == 'olivaceous')]<-'olivaceus'
gra$Species[which(gra$Species == 'velliferum')]<-'veliferum'

## add genus info from UVC list
gra$Genus<-sp$genus[match(gra$Species, sp$sp)]

## fix entries without minutes

gra$Phase<-NA
gra$dataset<-'GBR-Landry'
gra$Region<-'GBR'
colnames(gra)[colnames(gra) == 'Size']<-'TL'
colnames(gra)[colnames(gra) == 'Time']<-'time'
colnames(gra)[colnames(gra) == 'minutes']<-'min'
colnames(gra)[colnames(gra) == 'seconds']<-'sec'
colnames(gra)[colnames(gra) == 'Total_Bites']<-'Bites'
colnames(gra)[colnames(gra) == 'Bites_per_minute']<-'Bite.rate'
colnames(gra)[colnames(gra) == 'Site']<-'Reef'

## reorder columns
gra <- gra[, c(12,1,9,2,10,3,7,5,6,4,8,11)]


# ------------------------------------ #
	### MERGE DATASETS ###
# ------------------------------------ #
bite<-rbind(redsea, indonesia, gbr, gra)

## add species column
bite$sp<-with(bite, paste(Genus, Species))
## in UVC data?
bite$UVC<-ifelse(bite$sp %in% sp$species, 'TRUE', 'FALSE')
# add FG information
bite$FG<-sp$FG[match(bite$sp, sp$species)]


uniques(bite$sp[bite$UVC == TRUE]) ## 39 species
uniques(bite$sp[bite$UVC == FALSE]) ## 8 species

# > unique(bite$sp[bite$UVC == FALSE]) ## 10 species
#  [1] "Chlorurus genozonatus" "Chlorurus gibbus"      "Scarus ferrugenius"   
#  [4] "Scarus fuscopurpureus" "NA quoyi"              "NA russelli"          
#  [7] "NA "                   "NA muricatum"         

## double check missing species are not in UVC under a different spelling
sp[grepl('genoz', sp$sp),]
sp[grepl('gib', sp$sp),]
sp[grepl('ferr', sp$sp),]
sp[grepl('fusco', sp$sp),]
sp[grepl('quoy', sp$sp),]
sp[grepl('russ', sp$sp),]
sp[grepl('muri', sp$sp),]

## how many species in UVC are not in bites?
missing<-sp[!(sp$species %in% bite$sp),] ### 63 species
with(missing, table(FG))


## add time format for plotting
bite$total.sec<-bite$min * 30 + bite$sec

## checked. now save bite, filtered for UVC species
bite<-bite %>% filter(UVC == TRUE)
write.csv(bite, file = 'data/bite-rates/bite_rate_clean.csv')
