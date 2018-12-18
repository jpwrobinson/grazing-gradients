
library(tidyverse)
library(funk)
library(scales)
library(here)
theme_set(theme_sleek())
setwd(here('grazing-gradients'))


# data load
load("data/wio_herb_benthic_merged.Rdata")
# estimate mean biomass per site per FG
h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
	mutate(unique.transect = paste(unique.id, transect, sep='-')) %>%
  ## sum biomass per FG in each transect
        group_by(unique.transect, species) %>%
          summarise(abund = length(abundance.500m2)) #%>%
  ## mean species abundass across transects at each site
          # group_by(unique.id, species) %>%
          # summarise(abund = round(mean(abund)))

h <- pred %>% filter(FG == 'Herbivore Scraper') %>% 
  ## sum biomass per FG in each transect
        group_by(unique.id, species) %>%
          summarise(abund = length(abundance.500m2)) #%>%
  ## mean species abundass across transects at each site
          # group_by(unique.id, species) %>%
          # summarise(abund = round(mean(abund)))

samples<-pred %>% filter(FG == 'Herbivore Scraper') %>% 
  group_by(unique.id) %>%
  summarise(samples = uniques(transect)) 

## change names for colnames
com.mat<-tidyr::spread(h, species, abund)
# com.mat<-janitor::clean_names(com.mat)
rows<-com.mat[,1]
## drop cols
com.mat<-com.mat[, -c(1)]

## fill NAs
com.mat[is.na(com.mat)]<-0
## matrix format

com.mat<-as.matrix(com.mat)
dim(com.mat)
## transpose
com.mat<-t(com.mat)
colnames(com.mat)<-rows$unique.id

## import packages
library(iNEXT)

## coverage-based rarefaction
# ch<-iNEXT(com.mat, q=0, datatype="abundance")
# sites<-ch$DataInfo
# sites$unique.id<-str_split_fixed(sites$site, '\\-', 2)[,1]

# ## select only sites with at least 50% coverage & more than 1 species
# sub<-sites[sites$SC> 0.6 & sites$S.obs > 1,]
# uniques(sub$unique.id) ## ok all site sincluded


# ## subset com. matrix for sites with 50% coverage & more than 1 species
# ut<-unique(sub$site)
# com.mat2<-com.mat[,colnames(com.mat) %in% ut]
# rownames(com.mat2)<-NULL
# temp<-apply(com.mat2,2, as.list)
# temp<-lapply(temp,unlist)


# temp<-temp[c(250:255)]
# temp<-data.frame(temp)
# temp<-droplevels(temp)
# com.mat2[,250]
# ## now point estimates up to 98.5% coverage
# t<-estimateD(temp, datatype='abundance', base='coverage', level = NULL)
# head(t)
# com.mat2[,colnames(com.mat2) == '2011.Cousin Granite-4']

# divs$site<-sub$site
# divs$unique.id<-sub$unique.id

# richness<-divs %>%
#        select(unique.id, qD) %>%
#        group_by(unique.id) %>%
#        summarise_all(funs(mean, se))


rownames(com.mat)<-NULL
com.mat<-rbind(samples$samples, com.mat)
temp<-apply(com.mat,2, as.list)
temp<-lapply(temp,unlist)
t<-estimateD(temp, datatype='abundance', base='size', level = NULL)
t<-t %>% filter(order == 0)

write.csv(t, file = 'results/rarefied_richness_scrapers.csv')

## compare to freq divs
load(file = 'results/scraper_attributes.Rdata')
diversity.preds$estimated<-t$qD[match(diversity.preds$unique.id, t$site)]
diversity.preds$region<-pred$dataset[match(diversity.preds$unique.id, pred$unique.id)]

with(diversity.preds, cor(richness, estimated))
ggplot(diversity.preds, aes(estimated, richness, col=region)) + geom_point() #+ geom_abline(intercept=0, slope=1) + lims(x = c(2, 16), y = c(2,16))
ggplot(diversity.preds, aes(mean.abund, estimated, col=region)) + geom_point()
ggplot(diversity.preds, aes(log10(mean.biom), estimated, col=region)) + geom_point()
ggplot(diversity.preds, aes(scraping, estimated, col=region)) + geom_point()

pairs2(select_if(diversity.preds, is.numeric),
  lower.panel = panel.cor, upper.panel = panel.smooth2, diag.panel=panel.hist)
