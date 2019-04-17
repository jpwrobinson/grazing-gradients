
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)
theme_set(theme_sleek())

setwd(here('grazing-gradients'))

pdf(file = "figures/figure4_decoupling.pdf", width=8, height=4)

## load models and predictions. tidy up to merge
load("results/models/cropper_function.Rdata")
grazers<-h
grazers$grazef<-grazers$cropping.gram.ha
grazers$cropping.gram.ha<-NULL
grazers$sp <- 'grazers'

m.graze<-glmer(cropping.gram.ha ~ scale(biom) + (1 | dataset/reef), h, family='Gamma'(link = 'log'))
grazers$resid<-resid(m.graze)
r2marg.grazer<-rsquared(m.graze)$Marginal

save(grazers, file= 'results/models/cropper_function_resid.Rdata')

load("results/models/scraper_function.Rdata")
scrapers<-h
scrapers$grazef<-scrapers$scraping
scrapers$scraping<-NULL
scrapers$sp <- 'scrapers'

m.scrape<-glmer(scraping ~ scale(biom) + (1 | dataset/reef), h, family='Gamma'(link = 'log'))
scrapers$resid<-resid(m.scrape)
r2marg.scraper<-rsquared(m.scrape)$Marginal

save(scrapers, file= 'results/models/scraper_function_resid.Rdata')


load("results/models/browser_function.Rdata")
browsers<-h
browsers$grazef<-browsers$browsing
browsers$browsing<-NULL
browsers$sp <- 'browsers'

m.browse<-glmer(browsing ~ biom + (1 | dataset/reef), h, family='Gamma'(link = 'log'))
browsers$resid<-resid(m.browse)
r2marg.browser<-rsquared(m.browse)$Marginal

df<-rbind(grazers, scrapers, browsers)
r2<-data.frame(label = c(r2marg.grazer, r2marg.scraper, r2marg.browser), sp = c('grazers','scrapers', 'browsers'))
#r2$lab<-paste(expression(paste('R'^2,' = ', round(r2$label, 2))))
r2$label<-round(r2$label, 2)


## setup formatting information
linewidth = 4
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('grazers' = pal[5], 'scrapers' = pal[12], 'browsers' = pal[18])
theme_set(theme_sleek())

function_names <- list(
  'grazers'=expression(paste("Algal consumption (g ha"^-1,"min"^-1, ')')),
  'scrapers'=expression(paste('Area grazed (m'^2,' ha'^-1, 'min'^-1, ')'))
  #'browsers'="mass-standardized bites"
)

func.labels <- function(variable,value){
  return(function_names[value])
}

panel_labs <- data.frame(
  sp=c('grazers','scrapers'),#,'browsers'),
  # lab=c('b','c',"a")
  lab=c('A','B')
)

## drop browsers
df<-droplevels(df[!df$sp == 'browsers',])
r2<-r2[!r2$sp == 'browsers',]

ggplot(df, aes(biom, grazef, col=sp)) + 
        geom_point(alpha=0.5, size=3,aes(shape=dataset)) +
        facet_wrap(~ sp, scales= 'free',labeller=func.labels) +
  labs(title = "") +
  annotate("text", x = -Inf, y = Inf,hjust=1,vjust = -1.5, fontface=2, label = c("A", ""),size=6) +
  annotate("text", x = -Inf, y = Inf,hjust=1,vjust = -1.5, fontface=2, label = c("", "B"),size=6) +
  scale_color_manual(values = cols.named) +
  scale_x_continuous(label=comma) +
  scale_y_continuous(label=comma) +
    coord_cartesian(clip = "off") +
  # geom_text(data = panel_labs, aes(x = 0, y = Inf, label=lab),col='black', size=5.5, fontface=2, hjust=0.4, vjust=1.4) +
  guides(col=F,shape=guide_legend(nrow=4,byrow=TRUE,override.aes = list(alpha=0.5, col='black', size=3))) +
  theme(legend.position =c(0.92, 0.3), legend.title=element_blank(),
    strip.text.x = element_text(size=12),
    legend.text=element_text(size=12, colour='black'),
    legend.key.size = unit(0.5, "cm"),
    axis.text=element_text(size=14),
                axis.title=element_text(size=14)) +
  xlab(expression(paste("biomass, kg ha"^-1))) + ylab("Grazing rate")  +
  geom_text(data=r2, aes(Inf, Inf, label=paste0("R", "^", "2", "==", label)), size=5, fontface=2,vjust=2, hjust=2, parse=TRUE) 
  # geom_text(data=panel_labs, aes(Inf, Inf, label=panel_labs))

dev.off()
