



##############################################
#### Define SAD truncation function (Nadarajah and Kotz 2006):
# n = number of "individuals" sampled from discrete probability density function (our SAD)
# spec = distribution to be truncated
# a,b = upper and lower species limit of SAD to truncate over

rtrunc <- function(n, spec, a = -Inf, b = Inf, ...){
    x <- u <- runif(n, min = 0, max = 1)
    x <- qtrunc(u, spec, a = a, b = b,...)
    return(x)
}
qtrunc <- function(p, spec, a = -Inf, b = Inf, ...){
    tt <- p
    G <- get(paste("p", spec, sep = ""), mode = "function")
    Gin <- get(paste("q", spec, sep = ""), mode = "function")
    tt <- Gin(G(a, ...) + p*(G(b, ...) - G(a, ...)), ...)
    return(tt)
}


##############################################
#### Define function to simulate evenness over defined abundance and richness
# richness_vector = vector of richnesses to iterate over
# site_vector = vector of abundances to iterate over
# reps = number of repetitions to iterate through and calculate means (& SD) from
# distrib = truncated distribution to pull species abundances from
#### Any distribution in R stats package (i.e. "lnorm", "exp", "geom", "norm", etc.) can be used
#### Must specify the inputs (i.e. mean, shape, rate) for given distribution correctly
simulate_scraping <- function(richness_vector, bite.pred=freq, reps = 99, distrib = "lnorm", ...){
  
  scraping_matrix <- matrix(nrow = length(richness_vector), ncol = 2)
  
    i <- 1
    repeat{
      scraping <- vector()
      richnesses <- vector()
      iteration <- 0
      repeat{

      	# Define a community, selected at random from SAD, which max richness = lim
      	lim=max(richness_vector)
      	community <- ceiling(rtrunc(n = 1000,
      	 				   spec = 'lnorm', a = 0, b = lim))

		corrector<-data.frame(sp = 1:lim, bite=bite.pred[sample(1:29,lim, replace=F),'bite.rate'])
		comm.scraper<-corrector$bite[match(community, corrector$sp)]
        # Calculate scraping of community, bind to evenness of other scraping
        scraping <- c(scraping, sum(comm.scraper))

        # Calculate mean and standard error of community evennesses
      	scraping_matrix[i,1] <- mean(scraping)
      	scraping_matrix[i,2] <- se(scraping)
        # Iterate until reaching desired repetitions
        if(iteration == reps) {break}
        iteration <- iteration + 1
      }

       if(i == length(richness_vector)) {break}
      i <- i + 1
    }  
     return(list(mean = scraping_matrix[,1], se = scraping_matrix[,2], richness = richness_vector))
     }

sim<-simulate_scraping(1:29, reps=999)
df<-data.frame(mean = sim$mean, se = sim$se, richness = 1:29)


ggplot(df) + 
	geom_pointrange(aes(richness, mean, ymax = mean+2*se, ymin= mean-2*se)) +
	labs(x = 'Species richness', y = 'Bites per min') + 
	scale_x_continuous(breaks= seq(1, 29, 1)) + 
	scale_y_continuous(labels=comma)


