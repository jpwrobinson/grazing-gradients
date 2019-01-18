

library(here)
library(tidyverse)
library(rethinking)
library(tidybayes)
setwd(here('grazing-gradients/'))
theme_set(theme_bw())

## testing rat effect on scraping function
load(file = 'results/models/scraper_function.Rdata')
pred<-h %>% filter(date %in% c(2014, 2017) & dataset == 'Seychelles') %>% as.data.frame()

## create dummy numeric variables
pred$bleaching<-ifelse(pred$date == 2014, 1, 0) ### nobleaching = 0, bleaching = 1
## create ran effect id
pred$site_yr<-with(pred, paste(date, site, sep = '_'))
## add regime predictor
load('data/raw-data/SEY_UVC_fish_clean_1994-2017.Rdata')
pred$regime<-fish$state[match(pred$site, fish$Location)]
pred$state<-with(pred, ifelse(regime == 'Shifted', 0, 1))


m1 <- map2stan(
	alist(
	    scraping ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[site] + #ar2[site_yr] + ## intercept and randos for site & transects by site + year
	    b1*bleaching + ### bleaching effect
	    b2*state + ### state effect
	    b3*state*bleaching, ### bleaching*state interaction
	    a ~ dnorm(0, 10),
	    c(ar)[site] ~  dnorm(0, sigmar1),
	    # c(ar2)[site_yr] ~  dnorm(0, sigmar2),
	    c(b1, b2, b3) ~ dnorm(0, 10),
	    c(sigmar1, sigmar2) ~ dcauchy(0, 1),
	    scale ~ dexp(2)
	), data=pred, iter=3000, chains=1)

save(m1, pred, file = 'results/bleaching/scraping_change_bleaching_2016.Rdata')

# rerun model above if required for results - 3 chains, more iterations
load(file = 'results/bleaching/scraping_change_bleaching_rats.Rdata')
precis(m1)



## Testing model - how does bleaching * rats interact to determine scraping function?
pdf(file = 'figures/bleaching/sey_bleachingeffect_scraping_2016.pdf', height= 7, width=6)

a_site_zeros <- matrix(0,1000,21)
a_siteyr_zeros <- matrix(0,1000,42)

## bleaching = 1, recovering = 1
mat<-expand.grid(bleaching = c(0,1), state=c(0, 1), 
	site = 'Cousin Carbonate', site_yr='1994_Cousin Carbonate')

## predict rat + bleaching effects, holding island and atoll effects to zero
mu<-link(m1, data = mat, n=1000, replace=list(ar=a_site_zeros))#, ar2=a_siteyr_zeros))


pred.scrape<-apply(mu, 2, mean)
pred.PI <- apply(mu , 2 , HPDI , prob=0.95)
pred.PI.50 <- apply(mu , 2 , HPDI , prob=0.50)

plotter<-data.frame(
		mu = pred.scrape, 
		ui = pred.PI[2,],
		li = pred.PI[1,],
		ui50 = pred.PI.50[2,],
		li50 = pred.PI.50[1,],
		bleaching = c('Pre-bleaching', 'Post-bleaching', 'Pre-bleaching', 'Post-bleaching'),
		regime = c('Regime-shifted','Regime-shifted', 'Recovering','Recovering'),
		bl.num=c(0, 1, 0, 1))

ggplot(plotter, aes(regime, mu, col=factor(bl.num))) + 
			geom_point( position = position_dodge(width=0.1)) + 
			geom_pointrange(aes(ymin = li, ymax= ui),  position = position_dodge(width=0.1)) +
			geom_pointrange(aes(ymin = li50, ymax= ui50),size=2,  position = position_dodge(width=0.1)) +
			labs(y = 'Scraping function', x = '', title = 'Bleaching effect on scraping function') +
			theme(legend.position = c(0.8, 0.9), legend.title=element_blank()) +
			scale_color_discrete(labels = c('Pre-bleaching', 'Post-bleaching'))
			#scale_x_discrete(labels=unique(plotter$regime), lim=c(-0.2, 1.2))


# post<-as.data.frame(extract.samples(m1)) %>% gather(param, dist) %>%
# 		filter(param %in% c('a',  'b1')) %>% droplevels()
# ylabs<-c('Intercept',  'Bleaching')

# params0<-ggplot(post, aes(x = dist, y = param)) + 
# 		geom_halfeyeh(size=0.5, .width=0.95, fill=NA, density.color=NA) + 
# 		geom_halfeyeh(size=5, .width=0.50, fill=NA, density.color=NA) + 
# 		geom_vline(xintercept=0, linetype='dashed') +
# 		labs(y = '', x ='', title = 'Parameter effect sizes: bleaching effect on scraping') +
# 		# scale_x_continuous(breaks=seq(-10, 6, 2), lim = c(-10, 6)) +
# 		scale_y_discrete( labels=ylabs, position='right') + 
# 		theme(axis.text.y=element_text(size=10, colour='black'),
# 				axis.text.x=element_text(size=10, colour='black'),
# 				panel.border = element_blank(), 
# 				panel.grid.major = element_blank(),
# 				panel.grid.minor = element_blank(),
#  				axis.line = element_line(colour = "black")); params0


dev.off()