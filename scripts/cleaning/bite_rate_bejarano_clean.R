rm(list=ls())
setwd('/Users/robins64/Documents/git_repos/grazing-gradients')
library(tidyverse); theme_set(theme_bw()); library(chron)
uniques<-function(x){length(unique(x))}


## read sonja raw bite obs
bite<-read.csv('data/bite-rates/raw/bejarano_biterates_raw.csv', stringsAsFactors=F)

## drop fish without time data
bite <- bite[!bite$exit.time == 'not recorded',]
## drop no fish obs
bite <- bite[!bite$exit.time == 'no fish',]
bite <- bite[!bite$species == 'no fish',]

## drop uniformative columns
bite$part<-NULL
bite$seg<-NULL
bite$life.phase<-NULL
bite$size.SL<-NULL
bite$surveyor<-NULL

## fix numeric columns
bite$size.tl<-as.numeric(bite$size.tl)

## rename families
bite$Family<-NA
bite$Family[bite$family=='aca']<-'Acanthurid'
bite$Family[bite$family=='sig']<-'Siganid'
bite$Family[bite$family=='sca']<-'Scarid'

## estimate video time; first convert to time format
bite$time.entry<-as.character(bite$time.entry)
bite$time.entry<-ifelse(nchar(bite$time.entry)==7, paste0('0', bite$time.entry), bite$time.entry)
bite$time.entry<-chron(times=bite$time.entry)

bite$exit.time<-as.character(bite$exit.time)
bite$exit.time<-ifelse(nchar(bite$exit.time)==7, paste0('0', bite$exit.time), bite$exit.time)
bite$exit.time<-chron(times=bite$exit.time)

# diff between start and end observation
bite$survey.time<-with(bite, exit.time - time.entry)
## total seconds of survey time
bite$survey.secs<- as.numeric(seconds(bite$survey.time))


## estimate bites per hour
bite$bite.rate<-with(bite, as.numeric(as.character(total.bites))/survey.secs*60*60)
bite<-bite[!is.infinite(bite$bite.rate),]
aggregate(bite.rate ~ Family, bite, mean)

## SUMMARIES
unique(bite$species) ## n = 22
table(bite$Family)
hist(bite$survey.secs)



pdf(file='figures/explore/bite_rates_bysize.pdf')
ggplot(bite, aes(size.tl, bite.rate)) + geom_point() + facet_wrap(~Family) + ylim(0, 25000) + labs(x='total length (cm)', y = 'bites per hour')
ggplot(bite[bite$Family == 'Acanthurid',], aes(size.tl, bite.rate)) + geom_point() + facet_wrap(~species) + ylim(0, 25000) + labs(x='total length (cm)', y = 'bites per hour')
ggplot(bite[bite$Family == 'Scarid',], aes(size.tl, bite.rate)) + geom_point() + facet_wrap(~species) + ylim(0, 25000) + labs(x='total length (cm)', y = 'bites per hour')
ggplot(bite[bite$Family == 'Siganid',], aes(size.tl, bite.rate)) + geom_point() + facet_wrap(~species) + ylim(0, 25000) + labs(x='total length (cm)', y = 'bites per hour')
dev.off()

pdf(file='figures/explore/bite_rates_byspecies.pdf')
sp<-data.frame(table(bite$species))
sp$Family<-bite$Family[match(sp$Var1, bite$species)]
ggplot(sp, aes(reorder(Var1,Freq), Freq, fill=Family)) + 
	geom_bar(stat = 'identity') + coord_flip() + labs(x = 'Species', y = 'Number of observations') +
	theme(legend.position = c(0.75, 0.25))
dev.off()


