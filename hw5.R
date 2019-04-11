rm(list = ls())
setwd("G:\\math\\640")


fx <- function(x){ ifelse( x < 0 , 0,
	ifelse( 0 <= x & x < 0.5 ,  4*x ,
	ifelse( 0.5 <= x & x <= 1, 4-4*x,  
	ifelse(  x < 1, 0, 0 )
)))}


### U(0,1) ###
curve(fx, from = 0-.05, to = 1+.05)
M <- 2
curve(M *dunif(x, 0, 1), add=TRUE, col="green", lwd=2)

theta	<- vector(length = 1000)
arr <- NULL
t <- 1
count	<- 1

while(t < 1000){
 
	tb	<- runif(1)
	U	<- runif(1)
 	
	r <- fx(tb) / (M*dunif(tb))
	if(U < r){
		theta[t] <- tb
		t <- t + 1	
	points( tb , M*U , col = "red" , pch = 16) } else {
	points( (tb) ,M*U , col = "blue" , pch = 4) }
	count	<- count + 1
	
}



t/count
mean(theta)


### Beta(2,2) ###
curve(fx, from = 0-.05, to = 1+.05 )
M <- fx(.5) / ( (0.5)*(1-0.5) * (6) )
curve(M *dbeta(x, 2, 2), add=T, col="darkblue", lwd=2)


theta	<- vector(length = 1000)
arr <- NULL
t <- 1
count	<- 1

while(t < 100){
 
	tb	<- rbeta(1,2,2)
	U	<- runif(1)
 	
	r <- fx(tb) / (M*dbeta(tb,2,2))
	if(U < r){
		theta[t] <- tb
		t <- t + 1	
	points( tb , M*U*dbeta(tb,2,2) , col = "red" , pch = 16) } else {
	points( (tb) ,M*U*dbeta(tb,2,2) , col = "blue" , pch = 4) }
	count	<- count + 1
	
}



t/count
#plot(density(theta), lwd = 8, col = 'blue')
#curve(dbeta(x, 6, 3), add = TRUE, col = 'red', lwd = 8)
mean(theta)

 

#####

 
library(mvtnorm)
library(MCMCpack)
library(mcmcplots)
require(invgamma)

setwd("G:\\math\\640")

hers	<- read.table('hersreg.txt', header = TRUE)
tail(hers)
colnames(hers)[1] <- "y" 

hers0 <- hers[ which(hers[ ,2] == 0), 1:2]
 
B <- 10000

ss <- Alpha <- mu <- rep(NA,B)
ss[1] <- Alpha[1] <- mu[1] <- 1
v <- 1
n0 <- nrow(hers0)

set.seed(32717)
for (t in 2:B) { 
  Alpha[t] <- rinvgamma(1, (n0)/2 + .0001 ,  
	.0001 + (1/ 2 *ss[t-1])  * sum( (hers0$y - mu[t-1] )^2 ) )
  mu[t] <- rnorm(1, mean(hers0$y) , sqrt( Alpha[t]*ss[t-1] / n0) )
  ss[t] <- rinvgamma( 1 , (n0/2)+.0001 ,  .0001 + (1/(2*Alpha[t])) *
		 sum( ( hers0$y - mu[t] )^2 )   )
}
 
# Burn in
Alpha <- tail(Alpha,B/2)
mu <- tail(mu,B/2)
ss <- tail(ss,B/2)

mcmcplot1(  matrix( ss, ncol = 1))
mcmcplot1(  matrix( Alpha, ncol = 1))
mcmcplot1(  matrix( mu, ncol = 1))
 
hers1 <- hers[ which(hers[ ,2] == 1), 1:2]

ss <- Alpha <- mu <- rep(NA,B)
ss[1] <- Alpha[1] <- mu[1] <- 1
v <- 1
n <- nrow(hers1)

set.seed(32717)
for (t in 2:B) { 
  Alpha[t] <- rinvgamma(1, (n)/2 + .0001 ,  
	.0001 + (1/ 2 * ss[t-1])  * sum( (hers1$y - mu[t-1] )^2 ) )
  mu[t] <- rnorm(1, mean(hers1$y) , sqrt( Alpha[t]*ss[t-1] / n) )
  ss[t] <- rinvgamma( 1 , (n/2)+.0001 ,  .0001 + (1/(2*Alpha[t])) *
		 sum( ( hers1$y - mu[t] )^2 )   )
}
 
# Burn in
Alpha <- tail(Alpha,B/2)
mu <- tail(mu,B/2)
ss <- tail(ss,B/2)

mcmcplot1(  matrix( ss, ncol = 1))
geweke.diag(mcmc(ss))

mcmcplot1(  matrix( Alpha, ncol = 1))
geweke.diag(mcmc(Alpha))

mcmcplot1(  matrix( mu, ncol = 1))
geweke.diag(mcmc(mu))


fy <- function(y,mu,ss,n)(  ( (1+ (1/(n-1) * ((y-mu)^2/(ss))))^(-n/2)  ) )
fy2 <- function(y,mu,ss,n){  (-n^2/2) + ( (n/ss*(n-1)) * sum((y-mu)^2)  ) }
 
fyy <-  function(y)(  ((1+ (1/(nrow(hers1)-1) * (
	(y- mean(hers1$y))^2/(var(hers1$y)))))^(-nrow(hers1)/2)  )  /
	dcauchy(y,median(hers1$y),sd(hers1$y) ) )
 


fyy <-  function(y)(  ((1+ (1/(nrow(hers1)-1) * (
  (y- mean(hers1$y))^2/(var(hers1$y)))))^(-nrow(hers1)/2)  )  /
    dcauchy(y,median(hers1$y),sd(hers1$y) ) )

optimize( fyy , interval=c(-200, 200), maximum=T)

curve(fy(x, mean(hers1$y) , var(hers1$y), nrow(hers1) ), 
      from = -200 , to = 200 , ylim=c(0 ,1.25)  )
curve(  135.7294 *  dcauchy(x,median(hers1$y), sd(hers1$y)),  add = T  )


 