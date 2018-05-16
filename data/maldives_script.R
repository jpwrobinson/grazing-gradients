load('data/wio_gbr_herb_master.Rdata')
str(herb) ## check structure of data frame
unique(herb$dataset) ## check regions in herb

## subset to maldives data only
maldives <- herb[herb$dataset == 'Maldives',]

## write to csv
write.csv(maldives, file = 'data/maldives_master.csv')


setwd("~/Box Sync/PhD Feedbacks/Git Repository/grazing-gradients/data")
load('maldives_master.csv')
maldives <- maldives_master

str(maldives)
unique(maldives$FG)
unique(maldives$family)
unique(maldives$species)
unique(maldives$habitat)
unique(maldives$management)

hist(maldives$biomass.kgha[maldives$FG=="Herbivore Grazer"], 
     breaks = 30, main = "Grazers")
hist(maldives$biomass.kgha[maldives$FG=="Herbivore Scraper"], 
     breaks = 60, main = "Scrapers")
hist(maldives$biomass.kgha[maldives$FG=="Herbivore Browser"], 
     breaks = 30, main = "Browsers")

dotchart(maldives$biomass.kgha[maldives$FG=="Herbivore Grazer"], main = "Grazers")
dotchart(maldives$biomass.kgha[maldives$FG=="Herbivore Scraper"],main = "Scrapers")
dotchart(maldives$biomass.kgha[maldives$FG=="Herbivore Browser"], main = "Browsers")


#fancy James coding
hist(log2(maldives$biomass.kgha[maldives$FG=="Herbivore Grazer"]), main = "Grazers", axes =F)
axis(1, at=c(-1:5), labels=2^(c(-1:5))); axis(2)
hist(log2(maldives$biomass.kgha[maldives$FG=="Herbivore Scraper"]), main = "Scrapers", axes = F)
axis(1, at=c(-1:300), labels=2^(c(-1:300))); axis(2)
hist(log2(maldives$biomass.kgha[maldives$FG=="Herbivore Browser"]), main = "Browsers", axes = F)
axis(1, at=c(-1:30), labels=2^(c(-1:30))); axis(2)