

fx <- function(x){ ifelse( x < 0 , 0,
	ifelse( 0 <= x & x < 0.5 ,  4*x ,
	ifelse( 0.5 <= x & x <= 1, 4-4*x,  
	ifelse(  x < 1, 0, 0 )
)))}


### U(0,1) ###
curve(fx, from = 0-.05, to = 1+.05)

M <- 2
theta	<- vector(length = 1000)
arr <- NULL
t <- 1
count	<- 1

while(t < 100){
 
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
plot(density(theta), lwd = 8, col = 'blue')
curve(dbeta(x, 6, 3), add = TRUE, col = 'red', lwd = 8)
mean(theta)


### Beta(2,2) ###
curve(fx, from = 0-.05, to = 1+.05 )
M <- fx(.5) / ( (0.5)*(1-0.5) * (6) )

curve(M *dbeta(x, 2, 2), add=TRUE, col="darkblue", lwd=2)


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
plot(density(theta), lwd = 8, col = 'blue')
curve(dbeta(x, 6, 3), add = TRUE, col = 'red', lwd = 8)
mean(theta)

 

#####

 ### Example II.R8 ###
library(mvtnorm)
library(MCMCpack)
library(mcmcplots)
setwd("G:\\math\\640")

hers	<- read.table('hersreg.txt', header = TRUE)
tail(hers)
colnames(hers)[1] <- "x" 

hers1 <- hers[ which(hers[ ,2] == 1), 1:2]
hers0 <- hers[ which(hers[ ,2] == 0), 1:2]


 
B	<- 10000
A <- 2

ss <- Alpha <- mu <- rep(NA,B)
ss[1] <- Alpha[1] <- mu[1] <- 1
v <- 1
n0 <- nrow(hers0)


for (t in 2:B) { 
  Alpha[t] <- rinvgamma(1, (v+1)/2, v/ss[t-1] + 1/A^2 )
  mu[t] <- rnorm(1, mean(hers0$x) , sqrt( ss[t-1] / n0) )
  ss[t] <- rinvgamma( 1 , (n0+v)/2 ,  (.5   * sum( ( hers0$x - mu[t] )^2 ) )  - (v/Alpha[t]) )
}
 
Alpha <- tail(Alpha,B/2)
mu <- tail(mu,B/2)
ss <- tail(ss,B/2)



B	<- 10000
A <- 2

ss <- Alpha <- mu <- rep(NA,B)
ss[1] <- Alpha[1] <- mu[1] <- 1
v <- 1
n1 <- nrow(hers1)


for (t in 2:B) { 
  Alpha[t] <- rinvgamma(1, (v+1)/2, v/ss[t-1] + 1/A^2 )
  mu[t] <- rnorm(1, mean(hers1$x) , sqrt( ss[t-1] / n1) )
  ss[t] <- rinvgamma( 1 , (n1+v)/2 ,  (.5   * sum( ( hers1$x - mu[t] )^2 ) )  - (v/Alpha[t]) )
}

Alpha <- tail(Alpha,B/2)
mu <- tail(mu,B/2)
ss <- tail(ss,B/2)


