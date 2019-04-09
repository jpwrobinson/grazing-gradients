##### Benthic Regimes- Exploratory Analysis #####
# Use multivariate analysis to look at any benthic regimes across the entire dataset and within regions
# K-means clustering to assign benthic regimes 
# Created: September 24, 2018
# Updated: November 27, 2018
 

setwd("~/Documents/git_repos/grazing-gradients")
rm(list=ls()) 

# load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(grid)
library(gridExtra)
library(factoextra)
library(ggbiplot)
library(ggfortify)
library(purrr)
library(pca3d)
library(ggpubr)
source("scripts/functions/multiplot.R")

# load data
load("data/wio_herb_benthic_merged.Rdata")
ls()
############################################################


############################################################
#### Prepare Data for Multivariate Analysis ####

# Habitat Types: hard.coral macroalgae rubble substrate complexity
# Site= unique.id? or also by transect? and by date? 
# Need to make dataframe: site by habitat type 

data <- pred %>% dplyr::select(unique.id, dataset, hard.coral, macroalgae, rubble, 
                       substrate, complexity)
head(data) 

# Average across transects and date within a site
pred <- pred %>% 
  dplyr::group_by(unique.id,dataset) %>% 
  dplyr::summarize(hard.coral.mean = mean(hard.coral),
            macroalgae.mean = mean(macroalgae),
            rubble.mean = mean(rubble),
            substrate.mean = mean(substrate),
            complexity.mean = mean(complexity))
head(pred) # why did the date get added to unique.id?
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
             plot.margin=unit(c(1,1,1,1),"line"),
             plot.title=element_text(size=18),
             axis.title=element_text(size=14))

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

# Biplot: 2D
pdf(file='figures/explore/benthic_allregions_PCA_2D.pdf', height=7, width=10)

autoplot(pred_pca, loadings = TRUE, loadings.label = TRUE,
         data = pred, colour = 'dataset') + theme

dev.off()
############################################################

############################################################
##### K-means Clustering #####

# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- pred[-1]
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# 4 seems like the best cluster number 

# K-means clustering
set.seed(2) # the clustering changes each time you do it
km <- kmeans(pred[-1], 4,nstart=25) # make sure to save this so you are using the same object for everything
print(km)
autoplot(km, loadings=TRUE, loadings.label=TRUE, data = pred, # 4 means because it seems like the best from the previous plot
         label = FALSE, label.size = 4, frame = TRUE) + theme
ggsave(file='figures/explore/benthic_pca_kmeans.pdf', height=10, width=13)


# Save clusters to a dataframe
pca.kmeans <- data.frame(km$cluster)
head(pca.kmeans)
save(pca.kmeans, file="data/pca_allregions_kmeans4_clusters.Rdata")

# What do these clusters represent? 
pred[-1] %>%
  mutate(Cluster = km$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Cluster 1: high rubble
# Cluster 2: high macroalgae
# Cluster 3: high substrate
# Cluster 4: high coral 

# Test out 3 clusters (April 9, 2019)
set.seed(3) # the clustering changes each time you do it
km <- kmeans(pred[-1], 3,nstart=25) # make sure to save this so you are using the same object for everything
print(km)
autoplot(km, loadings=TRUE, loadings.label=TRUE, data = pred, # try 3 means
         label = FALSE, label.size = 4, frame = TRUE) + theme
ggsave(file='figures/explore/benthic_pca_kmeans_3clusters.pdf', height=10, width=13)

# Save clusters to a dataframe
pca.kmeans <- data.frame(km$cluster)
head(pca.kmeans)
save(pca.kmeans, file="data/pca_allregions_kmeans3_clusters.Rdata")

# What do these clusters represent? 
pred[-1] %>%
  mutate(Cluster = km$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Cluster 1: high rubble
# Cluster 2: high coral
# Cluster 3: high substrate

# macroalgae is similar across all 3 
############################################################


############################################################
### 3D Plot of Overal PCA ###
# 3D plot of PC1, PC2, PC3 (have to use a different package, makes an interactive plot)
pca3d(pred_pca, group=pred$dataset, legend="topright", biplot=TRUE) 
snapshotPCA3d(file="figures/explore/benthic_allregions_PCA_3D.png")

# Multipanel plot of 2D PC1, PC2, and PC3
p <- autoplot(pred_pca, loadings = TRUE, loadings.label = TRUE,
         data = pred, colour = 'dataset', x=1, y=2) + theme+ guides(colour=FALSE)
p1 <- autoplot(pred_pca, loadings = TRUE, loadings.label = TRUE,
         data = pred, colour = 'dataset', x=2, y=3) + theme+ guides(colour=FALSE)
p2 <- autoplot(pred_pca, loadings = TRUE, loadings.label = TRUE,
         data = pred, colour = 'dataset',x=1, y=3) + theme

# Have to use ggsave or you get a blank page at the beginning 
ggarrange(p, p1, p2, nrow=3, ncol=1,common.legend=TRUE, legend="bottom")
ggsave(file='figures/explore/benthic_allregions_PCA_2D_AllPCs.pdf', height=13, width=9)
############################################################



############################################################
### PCA For Each Region ###

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
p <- autoplot(chagos_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("Chagos")
p1 <- autoplot(GBR_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("GBR")
p2 <- autoplot(maldives_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("Maldives")
p3 <- autoplot(seychelles_pca, loadings = TRUE, loadings.label = TRUE) + 
  theme + ggtitle("Seychelles")

pdf(file='figures/explore/benthic_byregion_PCA_2D.pdf', height=12, width=18)

multiplot(p,p1,p2,p3, cols=2)

dev.off()
############################################################
