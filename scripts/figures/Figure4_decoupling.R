
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)
library(r2glmm)
library(here)
library(MuMIn)
library(piecewiseSEM)
library(lme4)
theme_set(theme_sleek())

setwd(here('grazing-gradients'))


## load models and predictions. tidy up to merge
load("results/models/cropper_function.Rdata")
grazers<-h
grazers$grazef<-grazers$cropping.gram.ha
grazers$cropping.gram.ha<-NULL
grazers$sp <- 'grazers'


load(file = 'results/cropper_attributes.Rdata')
## match in site level predictors
grazers$site.richness<-diversity.preds$richness[match(grazers$unique.id, diversity.preds$unique.id)]
grazers$site.size<-diversity.preds$mean.size[match(grazers$unique.id, diversity.preds$unique.id)]
grazers$site.lfi<-diversity.preds$mean.lfi[match(grazers$unique.id, diversity.preds$unique.id)]*100
grazers$log_biom<-log10(grazers$biom)


focal.crop<-funk::scaler(grazers, ID = c('dataset', 'reef', 'site', 'grazef','FG', 'unique.id', 'sp', 'date'), cats = TRUE)
m.graze<-glmer(grazef ~ log_biom * site.lfi + (1 | dataset/reef), focal.crop, 
      family='Gamma'(link = 'log'),
        na.action = na.fail)
hist(resid(m.graze))
summary(m.graze)
dredge(m.graze)
r2marg.grazer<-rsquared(m.graze)$Marginal
car::vif(m.graze)

## save AIC scores from top 7 models
m.table<-dredge(m.graze)
tab<-data.frame(m.table)
tab[is.na(tab)]<-0
tab$r2<-r2marg.grazer
write.csv(tab, 'results/tables/croppers_lfi_AICtable.csv')

## refit top model
m.graze<-glmer(grazef ~ log_biom + site.lfi + (1 | dataset/reef), focal.crop, 
      family='Gamma'(link = 'log'),
        na.action = na.fail)
summary(m.graze)
# save(grazers, file= 'results/models/cropper_function_resid.Rdata')

  

load("results/models/scraper_function.Rdata")
scrapers<-h
scrapers$grazef<-scrapers$scraping
scrapers$scraping<-NULL
scrapers$sp <- 'scrapers'

load(file = 'results/scraper_attributes.Rdata')
## match in site level predictors
scrapers$site.richness<-diversity.preds$richness[match(scrapers$unique.id, diversity.preds$unique.id)]
scrapers$site.size<-diversity.preds$mean.size[match(scrapers$unique.id, diversity.preds$unique.id)]
scrapers$site.lfi<-diversity.preds$mean.lfi[match(scrapers$unique.id, diversity.preds$unique.id)]*100

scrapers$log_biom<-log10(scrapers$biom)
focal.scrape<-funk::scaler(scrapers, ID = c('dataset', 'reef', 'site', 'grazef','FG', 'unique.id', 'sp', 'date'), cats = TRUE)

## check collinearity
mat<-focal.scrape %>% select(log_biom, site.lfi) 
pairs2(mat, diag.panel = panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth2)


m.scrape<-glmer(grazef ~ log_biom * site.lfi + (1 | dataset/reef), focal.scrape, 
      family='Gamma'(link = 'log'),
        na.action = na.fail)
dredge(m.scrape)
# scrapers$resid<-resid(m.scrape)
r2marg.scraper<-rsquared(m.scrape)$Marginal
summary(m.scrape)
hist(resid(m.scrape))

# save(scrapers, file= 'results/models/scraper_function_resid.Rdata')

## save AIC scores from top 7 models
m.table<-dredge(m.scrape)
tab<-data.frame(m.table)
tab[is.na(tab)]<-0
tab$r2<-r2marg.scraper
write.csv(tab, 'results/tables/scrapers_lfi_AICtable.csv')

### Now predict biomass and LFI effects

### CROPPERS
r2<-data.frame(r2beta(m.graze, method = 'nsj', partial = TRUE))

lim25<-(25 - mean(grazers$site.lfi)) / sd(grazers$site.lfi) 
lim75<-(75 - mean(grazers$site.lfi)) / sd(grazers$site.lfi) 

pred.c<-data.frame(site.lfi_raw = c(25, 75), site.lfi = c(lim25, lim75),
     log_biom = seq(min(focal.crop$log_biom), max(focal.crop$log_biom), length.out=300),
     log_biom_raw = seq(min(grazers$log_biom), max(grazers$log_biom), length.out=300),
     biom_raw = seq(min(grazers$biom), max(grazers$biom), length.out=300),
     dataset='Chagos', reef='Diego Garcia', fg = 'Croppers')
pred.c$pred.c<-predict(m.graze, newdata = pred.c, re.form = NA, type='response', se.fit=TRUE)$fit
pred.c$se<-predict(m.graze, newdata = pred.c, re.form = NA, type='response', se.fit=TRUE)$se.fit
pred.c$upper<-with(pred.c, pred.c + 2 * se)
pred.c$lower<-with(pred.c, pred.c - 2 * se)

## truncate predicted lines to max observed
little.lim<-max(grazers$log_biom[grazers$site.lfi <= 35])
big.lim<-max(grazers$log_biom[grazers$site.lfi >= 50])
pred.c<-pred.c[!(pred.c$site.lfi_raw == 25 & pred.c$log_biom_raw > little.lim),]
pred.c<-pred.c[!(pred.c$site.lfi_raw == 75 & pred.c$log_biom_raw > big.lim),]


ggplot() + geom_line(data=pred.c, aes(log_biom_raw, pred.c, group=site.lfi)) + 
  geom_ribbon(data=pred.c, aes(log_biom_raw, pred.c, ymax = upper, ymin = lower, group=site.lfi), alpha=0.2) +
  geom_point(data = grazers, aes(log_biom, grazef))

## SCRAPERS
r2<-rbind(r2, data.frame(r2beta(m.scrape, method = 'nsj', partial = TRUE)))

## get scaled LFI vals for 25% and 75% LFI
lim25<-(25 - mean(scrapers$site.lfi)) / sd(scrapers$site.lfi) 
lim75<-(75 - mean(scrapers$site.lfi)) / sd(scrapers$site.lfi) 

pred.s<-data.frame(site.lfi_raw = c(25, 75), site.lfi = c(lim25, lim75),
  log_biom = seq(min(focal.scrape$log_biom), max(focal.scrape$log_biom), length.out=300),
  log_biom_raw = seq(min(scrapers$log_biom), max(scrapers$log_biom), length.out=300),
  biom_raw = seq(min(scrapers$biom), max(scrapers$biom), length.out=300),
     dataset='Chagos', reef='Diego Garcia', fg = 'Scrapers')
pred.s$pred.s<-predict(m.scrape, newdata = pred.s, re.form = NA, type='response', se.fit=TRUE)$fit
pred.s$se<-predict(m.scrape, newdata = pred.s, re.form = NA, type='response', se.fit=TRUE)$se.fit
pred.s$upper<-with(pred.s, pred.s + 2 * se)
pred.s$lower<-with(pred.s, pred.s - 2 * se)

## truncate predicted lines to max observed
little.lim<-max(scrapers$log_biom[scrapers$site.lfi <= 35])
big.lim<-max(scrapers$log_biom[scrapers$site.lfi >= 75 & scrapers$biom < 3000])
pred.s<-pred.s[!(pred.s$site.lfi_raw == 25 & pred.s$log_biom_raw > little.lim),]
pred.s<-pred.s[!(pred.s$site.lfi_raw == 75 & pred.s$log_biom_raw > big.lim),]


ggplot() + geom_line(data=pred.s, aes(log_biom_raw, pred.s, group=site.lfi)) + 
  geom_ribbon(data=pred.s, aes(log_biom_raw, pred.s, ymax = upper, ymin = lower, group=site.lfi), alpha=0.2) +
  geom_point(data = scrapers, aes(log_biom, grazef, col=site.lfi)) +
  labs(color= 'Proportion of fish > 30 cm') +
  scale_colour_continuous() +
  theme(legend.position = c(0.2, 0.8))

## summary stat for croppers
p<-predict(m.graze, newdata = data.frame(
      log_biom = log10(mean(grazers$biom)),
      site.lfi = c(lim25, lim75),
      dataset = 'Chagos', reef = 'Diego Garcia'), re.form=NA)
(p[1]-p[2])/p[2]*100

## summary stat for scrapers
p<-predict(m.scrape, newdata = data.frame(
      log_biom = log10(mean(scrapers$biom)),
      site.lfi = c(lim25, lim75),
      dataset = 'Chagos', reef = 'Diego Garcia'), re.form=NA)
(p[1]-p[2])/p[2]*100

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

df<-rbind(pred.c, pred.s)
r2$fg<-c(rep('Croppers', 4), rep('Scrapers', 4))
r2<-r2[r2$Effect == 'Model',]
r2$label<-round(r2$Rsq, 2)

myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "BrBG")))

left<-ggplot() + geom_point(data = grazers[-which.max(grazers$log_biom),], aes(log_biom, grazef, col=site.lfi), alpha=0.8, size=2) +
  geom_line(data=pred.c, aes(log_biom_raw, pred.c, group=site.lfi, linetype=factor(site.lfi))) + 
  geom_ribbon(data=pred.c, aes(log_biom_raw, pred.c, ymax = upper, ymin = lower, group=site.lfi), alpha=0.2) +
  # labs(linetype= 'Fish > 30 cm') +
  # scale_linetype_manual(values = c(1,2), labels = c('25%', '75%')) +
  # facet_wrap(~ fg, scales= 'free') +
scale_colour_gradientn(colors = myPalette(10), breaks = c(0, 25, 50, 75, 100), labels = c('0%', '25%', '50%', '75%', '100%')) +
  scale_x_continuous(breaks = seq(1, 3, by = 1),
    labels = 10^seq(1, 3, by = 1)) +
  theme(legend.position = 'none',
    strip.text.x = element_text(size=12),
    legend.text=element_text(size=12, colour='black'),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    legend.key.size = unit(0.5, "cm"),
    axis.text=element_text(size=14),
                axis.title=element_text(size=14),
                plot.title=element_text(size = 12, color = 'black', hjust=0.5)) +
  xlab(expression(paste("biomass, kg ha"^-1))) + ylab(expression(paste("g C ha"^-1,"min"^-1)))  +
  labs(title='Croppers')
  # geom_text(data=r2[r2$fg == 'Croppers',], aes(Inf, Inf, 
  #     label=paste0("R", "^", "2", "==", label)), size=5, fontface=2,vjust=2, hjust=2, parse=TRUE) 


right<-ggplot() + geom_point(data = scrapers[-which.max(scrapers$log_biom),], aes(log_biom, grazef, col=site.lfi), alpha=0.8, size=2) +
  geom_line(data=pred.s, aes(log_biom_raw, pred.s, group=site.lfi, linetype=factor(site.lfi))) + 
  geom_ribbon(data=pred.s, aes(log_biom_raw, pred.s, ymax = upper, ymin = lower, group=site.lfi), alpha=0.2) +
  labs(color= 'Fish > 30 cm', linetype='') +
  scale_colour_gradientn(colors = myPalette(10), breaks = c(0, 25, 50, 75, 100), labels = c('0%', '25%', '50%', '75%', '100%')) +
  scale_linetype_manual(values = c(1,2), labels = c('25%', '75%')) +
  guides(linetype=F) +
  scale_x_continuous(breaks = seq(1, 3, by = 1),
    labels = 10^seq(1, 3, by = 1)) +
  theme(legend.position = 'none',
    legend.key.size = unit(0.4, "cm"),
          plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    strip.text.x = element_text(size=12),
    legend.text=element_text(size=10, colour='black'),
    axis.text=element_text(size=14),
                axis.title=element_text(size=14),
                legend.title=element_text(size=10),
                plot.title=element_text(size = 12, color = 'black', hjust=0.5)) +
  xlab(expression(paste("biomass, kg ha"^-1))) + ylab(expression(paste('m'^2,' ha'^-1, 'min'^-1)))  +
  labs(title='Scrapers')
  # geom_text(data=r2[r2$fg == 'Scrapers',], aes(Inf, Inf, 
  #     label=paste0("R", "^", "2", "==", label)), size=5, fontface=2,vjust=2, hjust=2, parse=TRUE) 

leg<-ggplot() + 
  geom_line(data=pred.s, aes(log_biom_raw, pred.s, group=site.lfi, linetype=factor(site.lfi))) + 
  scale_linetype_manual(values = c(1,2), labels = c('25%', '75%')) +
  labs(linetype =  '') +
  theme(legend.key.size = unit(0.6, 'cm'))
legend1 <- lemon::g_legend(leg)

leg<-ggplot() + 
  geom_point(data = scrapers, aes(log_biom, grazef, col=site.lfi)) +
  scale_colour_gradientn(colors = myPalette(10), breaks = c(0, 25, 50, 75, 100)) +#, labels = c('0%', '25%', '50%', '75%', '100%')) +
  labs(colour =  '% Large fish')  +
  theme(legend.title=element_text(size=11, colour='black'),legend.text=element_text(size=10))
legend2 <- lemon::g_legend(leg)

pdf(file = "figures/figure4_decoupling_lfi.pdf", width=8, height=4)
left<-left + annotation_custom(grob = legend1, xmin = log10(80), xmax = log10(100), ymin = 4.5, ymax = 4.5) +
            annotation_custom(grob = legend2, xmin = log10(10), xmax = log10(50), ymin = 3, ymax = 4.5) 
cowplot::plot_grid(left, right, labels=c('A', 'B'), align= 'hv')
dev.off()
