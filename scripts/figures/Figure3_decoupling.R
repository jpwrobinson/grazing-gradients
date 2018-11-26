
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(here)
library(piecewiseSEM)
library(lme4)

setwd(here('grazing-gradients'))

pdf(file = "figures/figure3_decoupling.pdf", width=9, height=4)

## load models and predictions. tidy up to merge
load("results/models/cropper_function.Rdata")
grazers<-h
grazers$grazef<-grazers$cropping.gram.ha
grazers$cropping.gram.ha<-NULL
grazers$sp <- 'grazers'

m.graze<-lmer(cropping.gram.ha ~ biom + (1 | dataset), h)
grazers$resid<-resid(m.graze)
r2marg.grazer<-rsquared(m.graze)$Marginal

load("results/models/scraper_function.Rdata")
scrapers<-h
scrapers$grazef<-scrapers$scraping
scrapers$scraping<-NULL
scrapers$sp <- 'scrapers'

m.scrape<-lmer(scraping ~ biom + (1 | dataset), h)
scrapers$resid<-resid(m.scrape)
r2marg.scraper<-rsquared(m.scrape)$Marginal


load("results/models/browser_function.Rdata")
browsers<-h
browsers$grazef<-browsers$browsing
browsers$browsing<-NULL
browsers$sp <- 'browsers'

m.browse<-lmer(browsing ~ biom + (1 | dataset), h)
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
  'grazers'=expression(paste("algal consumption g ha"^-1,"min"^-1)),
  'scrapers'=expression(paste('area grazed m'^2,'ha'^-1, 'min'^-1)),
  'browsers'="mass-standardized bite rates"
)

func.labels <- function(variable,value){
  return(function_names[value])
}

# panel_labs <- list(
#   'grazers'='B',
#   'scrapers'='C',
#   'browsers'="A"
# )

ggplot(df, aes(biom, grazef, col=sp)) + 
        geom_point(alpha=0.5, aes(shape=dataset)) +
        facet_wrap(~ sp, scales= 'free',labeller=func.labels) +
  labs(title = "") +
  scale_color_manual(values = cols.named) +
  scale_x_log10(label=comma) +
  scale_y_continuous(label=comma) +
  guides(col=F) +
  theme(legend.position ='top', legend.title=element_blank()) +
  xlab(expression(paste("biomass kg ha"^-1))) + ylab("Function")  +
  geom_text(data=r2, aes(Inf, Inf, label=paste0("R", "^", "2", "==", label)), vjust=4, hjust=4, parse=TRUE) #+
  # geom_text(data=panel_labs, aes(Inf, Inf, label=panel_labs))

dev.off()
