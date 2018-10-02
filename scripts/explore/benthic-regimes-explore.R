##### Benthic Regimes- Exploratory Analysis #####
# Use multivariate analysis to look at any benthic regimes across the entire dataset and within regions
# Created: September 24, 2018
 

setwd("~/Documents/git_repos/grazing-gradients")
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(vegan)
library(grid)
library(gridExtra)
library(factoextra)
library(ggbiplot)
library(ggfortify)
library(purrr)

# load data
load("data/wio_herb_benthic_merged.Rdata")
ls()
head(pred)
dim(pred)
str(pred)
############################################################


############################################################
#### Prepare Data for Multivariate Analysis ####

# Habitat Types: hard.coral macroalgae rubble substrate complexity
# Site= unique.id? or also by transect? 
# Need to make dataframe: site by habitat type 

pred <-pred %>% select(unique.id,dataset,hard.coral, macroalgae, rubble, 
                       substrate, complexity, dataset)
head(pred) 

# Average across transects within a site
pred <- pred %>% 
  dplyr::group_by(unique.id,dataset) %>% 
  dplyr::summarize(hard.coral.mean = mean(hard.coral),
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
### Overall PCA (all regions) ###

# set blank theme
theme<-theme(panel.background = element_blank(),
             panel.border=element_rect(fill=NA),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background=element_blank(),
             axis.text.x=element_text(colour="black"),
             axis.text.y=element_text(colour="black"),
             axis.ticks=element_line(colour="black"),
             plot.margin=unit(c(1,1,1,1),"line"))

# PCA: 2 dimensions
pred_pca <- prcomp(pred[-1], # take out the dataset column
                   scale=TRUE) # scale the data
pred_pca

plot(pred_pca$x[,1], pred_pca$x[,2])

# Scree Plot
fviz_eig(pred_pca) 

# PCA: coloured by dataset
df_out <- as.data.frame(pred_pca$x) # convert to df for ggplot
df_out$group <- sapply(strsplit(as.character(pred$dataset), "_"), "[[", 1 )
head(df_out)
class(df_out)

# calculate percentage for each axis
percentage <- round(df_out$sdev / sum(df_out$sdev) * 100, 2)
percentage <- paste( colnames(df_out), "(", paste( as.character(percentage), "%", ")", sep="") )

p<-ggplot(df_out,aes(x=PC1,y=PC2, colour=group))
p<-p+geom_point() + theme + xlab(percentage[1]) + ylab(percentage[2])
p

# Biplot
autoplot(pred_pca, loadings = TRUE, loadings.label = TRUE,
         data = pred, colour = 'dataset') + theme

# K-means clustering with PCA (plots the 4 groupings on the PCA)
autoplot(kmeans(pred[-1], 4), data = pred, # 4 means because we have four island groups 
         label = TRUE, label.size = 3, frame = TRUE) + theme
kmeans(pred[-1], 4)
############################################################



############################################################
### PCA for each Region ###

# put back rownames for conversion
ha <- pred
ha$unique.id <- rownames(pred) # assign sites as rownames
head(pred)

# split into regions
haha <- ha %>% 
  group_by(dataset) %>% 
  do(data = (.)) %>% 
  select(data) %>% 
  map(identity)
head(haha)
str(haha)

chagos <- haha$data[[1]]
GBR <- haha$data[[2]]
maldives <- haha$data[[3]]
seychelles <- haha$data[[4]]

# Ready for vegan 
tidy.DATA = function(DATA){
  ha <- as.data.frame(DATA)
  rownames(ha) <- ha$unique.id # assign sites as rownames
  haha <- ha %>% select(-unique.id, -dataset) # get rid of redundant columns
  haha <- data.frame(haha)
  return(haha)
  
}

chagos <- tidy.DATA(chagos)
GBR <-tidy.DATA(GBR)
maldives <- tidy.DATA(maldives)
seychelles <- tidy.DATA(seychelles)

# PCA
chagos_pca <- prcomp(chagos, scale=TRUE) 
GBR_pca <- prcomp(GBR, scale=TRUE) 
maldives_pca <- prcomp(maldives, scale=TRUE) 
seychelles_pca <- prcomp(seychelles, scale=TRUE) 

# Scree plot
fviz_eig(chagos_pca) 
fviz_eig(GBR_pca) 
fviz_eig(maldives_pca) 
fviz_eig(seychelles_pca) 

# Biplot
autoplot(chagos_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("Chagos")
autoplot(GBR_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("GBR")
autoplot(maldives_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("Maldives")
autoplot(seychelles_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("Seychelles")
############################################################
