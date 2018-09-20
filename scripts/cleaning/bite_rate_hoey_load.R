
## Script to load Hoey bite rate datasets

setwd("~/Documents/git_repos/grazing-gradients")

# package loads
library(tidyverse)

redsea<-read.csv('data/bite-rates/raw/hoey_redsea.csv')
redsea$dataset<-'redsea'
redsea<-cbind(Region = redsea[,1], Reef = rep(NA,dim(redsea)[1]), redsea[,2:9])


indonesia<-read.csv('data/bite-rates/raw/hoey_indonesia.csv')
indonesia$dataset<-'indonesia'
indonesia$Water.temp<-NULL
colnames(indonesia)[10]<-'Bite.rate'

indonesia$Species<-str_split_fixed(indonesia$Species, '. ', 2)[,2]
head(indonesia)

gbr<-read.csv('data/bite-rates/raw/hoey_gbr.csv')
gbr$dataset<-'gbr'
gbr$TL.1<-NULL


bite<-rbind(redsea, indonesia, gbr)

head(redsea)