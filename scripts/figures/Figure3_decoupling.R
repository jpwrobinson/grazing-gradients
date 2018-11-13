
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
setwd(here('grazing-gradients'))

pdf(file = "figures/figure3_decoupling.pdf", width=9, height=4)

## load models and predictions. tidy up to merge
load("results/models/cropper_function.Rdata")
grazers<-h
grazers$grazef<-grazers$cropping.gram.ha
grazers$cropping.gram.ha<-NULL
grazers$sp <- 'grazers'

load("results/models/browser_function.Rdata")
browsers<-h
browsers$grazef<-browsers$browsing
browsers$browsing<-NULL
browsers$sp <- 'browsers'

df<-rbind(grazers, browsers)

## setup formatting information
linewidth = 4
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('grazers' = pal[5], 'scrapers' = pal[12], 'browsers' = pal[18])
theme_set(theme_sleek())

function_names <- list(
  'grazers'="algal consumption (g/ha/min)",
  'browsers'="mass-standardized bite rates"
)

func.labels <- function(variable,value){
  return(function_names[value])
}

ggplot(df, aes(biom, grazef, col=sp, shape=dataset)) + 
        geom_point(alpha=0.5) +
        facet_wrap(~ sp, scales= 'free',labeller=func.labels) +
  labs(title = "") +
  scale_color_manual(values = cols.named) +
  scale_x_log10(label=comma) +
  scale_y_continuous(label=comma) +
  guides(col=F) +
  theme(legend.position ='top', legend.title=element_blank()) +
  xlab("Biomass (kg/ha)") + ylab("Function") 

dev.off()
