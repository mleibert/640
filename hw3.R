rm(list = ls())
setwd("G:\\math\\640")
options(scipen=999)

dat <- read.table("incidenceUK.txt",header = T)

tail(dat)


n = nrow(dat)
k = 22
xybar <- mean( dat$male )
xxbar <- mean( dat$female )
Nsim <- 10000

set.seed(2020)
#male
xy <- rnorm( Nsim , (n*k-1) / (n * xybar ) , sqrt(n*k-1) / (n * xybar ) )
quantile( xy , probs = c(.5,.025,0.975) )

plot( density ( xy ) )

set.seed(2020)
#female
xx <- rnorm( Nsim , (n*k-1) / (n * xxbar ) , sqrt(n*k-1) / (n * xxbar ) )
quantile( xx  , probs = c(.5,.025,0.975) )
plot( density ( xy ) , xlim = c(.015,0.035) )
lines( density ( xx )  )
