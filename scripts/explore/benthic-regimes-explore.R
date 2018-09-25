##### Benthic Regimes- Exploratory Analysis #####
# Use multivariate analysis to look at any benthic regimes across the entire dataset and within regions
# Created: September 24, 2018
 
setwd("~/Documents/git_repos/grazing-gradients")
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)
library(grid)
library(gridExtra)

# load data
load("data/wio_herb_benthic_merged.Rdata")
ls()
head(pred)
dim(pred)
str(pred)
pred$unique.id
############################################################


############################################################
#### Prepare Data for Multivariate Analysis ####

# Habitat Types: hard.coral macroalgae rubble substrate complexity
# Site= unique.id? or also by transect? 
# Need to make dataframe: site by habitat type 

pred <-pred %>% select(unique.id, hard.coral, macroalgae, rubble, 
                       substrate, complexity, dataset)
head(pred) 

# Average across transects within a site
pred <- pred %>%
    group_by(unique.id) %>%
    summarise(hard.coral.mean = mean(hard.coral),
              macroalgae.mean = mean(macroalgae),
              rubble.mean = mean(rubble),
              substrate.mean = mean(substrate),
              complexity.mean = mean(complexity))
head(pred)
str(pred)

# Change rownames
pred <- data.frame(pred)
rownames(pred) <- pred$unique.id # assign sites as rownames
rownames(pred)
pred <-pred %>% select(-unique.id) # get rid of the redundant column
head(pred)

# Ready for vegan
pred<-data.frame(pred)
head(pred)
############################################################


############################################################
### Plot ###

df <- pred[-1]
head(df)

theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))

df_pca <- prcomp(df)
df_pca
plot(df_pca$x[,1], df_pca$x[,2])

df_out <- as.data.frame(df_pca$x)
df_out$group <- sapply(strsplit(as.character(pred$dataset), "_"), "[[", 1 )
head(df_out)


percentage <- round(df_pca$sdev / sum(df_pca$sdev) * 100, 2)
percentage <- paste( colnames(df_out), "(", paste( as.character(percentage), "%", ")", sep="") )

p<-ggplot(df_out,aes(x=PC1,y=PC2, colour=group))
p<-p+geom_point() + theme + xlab(percentage[1]) + ylab(percentage[2])
p

# Biplot
library("ggbiplot")
pca.bi <- prcomp(wine,scale.=TRUE)
p<-ggbiplot(wine.pca, obs.scale=1, var.scale=1, groups=wine.class, ellipse=TRUE, circle=TRUE)
p<-p+scale_color_discrete(name="")

