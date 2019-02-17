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







########################################################################

rm(list = ls())
dat <- read.table("coup1980.txt",header = T)
require(MASS)
tail(dat)

n = nrow(dat)
y <- dat[,2]
X <- as.matrix( dat[, - c(1,2 ) ] )
X <- cbind(1,X) ;   colnames(X)[1] <- "(Intercept)"
p <- ncol(X)
BHat <- solve( t(X)%*%X ) %*% t(X) %*% y


Nsim = 10000
set.seed(1980)
lambda <- rgamma(Nsim, (n-p)/ 2 , .5*t( y- X %*% BHat )%*% (y- X %*% BHat ))

set.seed(1980)
Beta <- matrix(NA, Nsim , p )
for( i in 1:Nsim){ Beta[i,] <- mvrnorm( 1, BHat, ( 1/lambda[i] ) * 
	solve( t(X)%*% X )    ) }

glm( logCoup~.   , data = dat[,-1 ] )
apply( Beta , 2 , mean) 

max( exp( predict( glm( logCoup~.   , data = dat[,-1 ] )  ) ) )*100

exp( t(apply( Beta , 2 , mean)) %*% c( 1 ,  1 , 72 , 25 )   )
exp( t(apply( Beta , 2 , mean)) %*% c( 1 ,  0 , 90 , 30*12)   )

 


