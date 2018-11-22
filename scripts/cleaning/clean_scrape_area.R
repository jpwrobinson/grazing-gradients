

library(here)
setwd(here('grazing-gradients'))

area<-read.csv('data/bite-rates/raw/scrape_size_raw.csv')
area$bite.area.mm2<-with(area, Bite.length..mm. * Bite.width..mm.)
head(area); dim(area) ## 1323 obs

## change column names
colnames(area)<-c('genus', 'species', 'phase', 'location', 'location2', 'date', 'TL', 'bitelength', 'bitewidth', 'bitearea')


write.csv(area, file='data/bite-rates/scrape_sizes.csv')


hist(area$bitearea)
hist(log(area$bitearea))
## will need gamma dist in models

## model testing here, migrate to Rmd when correct
area.prior<-mean(area$bitearea) ## 85.68

library(rethinking)

## drop zeroes
area <- area[area$bitearea > 0,] ## 253 obs
log(area.prior)

scrape.m2<-map2stan(
        alist(
          bitearea ~ dgamma2(mu, scale),
          log(mu) ~ a + B*TL, #+ X1[species] + X2[genus],
          #X1[species] ~ dnorm(0, sigmar),
          #X2[genus] ~ dnorm(0, sigmar2),
          a ~ dnorm(4.45, 5),
          B ~ dnorm(0, 2),
          scale ~ dexp(5)
          #c(sigmar, sigmar2) ~ dcauchy(0, 1)
        ),
        data=area, warmup = 1500, iter = 5000, chains =1, cores = 4)

precis(scrape.m2)
postcheck(scrape.m2)
plot(precis(scrape.m2))
plot(scrape.m2)

save(area, scrape.m2, file = 'results/models/area_scrapers.Rdata')

library(ggplot2)
ggplot(area, aes(TL, bitearea)) + geom_point()

pdf(file='figures/explore/scraper_area_byspecies.pdf')
theme_set(theme_bw())
ggplot(area, aes(TL, bitearea, col=genus)) + geom_point() + facet_wrap(~ species)
dev.off()