rm(list = ls())

require(MCMCpack)
setwd("G:\\math\\640")

Kumaraswamy <- function(x) { 2*2*x^(2-1)*(1-x^2)^(2-1) }

B <- 10000
xs <- ar <- rep(0, B)
ar <- vector("numeric", B)

Xs <- Ar <- list()

a <- c(1,2,2,3)
b <- c(1,1,2,2)

j = 2

for( j in 1:length(a)) {
	x  <- .5
	xs <- ar <- rep(0, B)
	set.seed(1218)
	for( i in 2:B){
		xstar	<- rbeta(1, a[j], b[j])
		rho <- (  Kumaraswamy(xstar) / Kumaraswamy(x)  ) * 
			( dbeta( x , a[j], b[j] )  /  dbeta( xstar, a[j], b[j] ) )
		rho <- min(1, rho )
	
		if ( runif(1) < rho ){ x <- xstar; ar[i] <- 1}
		xs[i] <- x }
	Xs[[j]] <- xs[-(1:(B/2))] 
	Ar[[j]] <- ar
	}

j = 4
 

par(mfrow=c(2,2) , mar=c(2.1,2.1,2.1,2.1) )
k=1; plot(cumsum(Xs[[k]] )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2,
	main = paste0("alpha = ",a[k], ", beta = ",b[k]) )
k=2; plot(cumsum(Xs[[k]] )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2,
	main = paste0("alpha = ",a[k], ", beta = ",b[k]) )
k=3; plot(cumsum(Xs[[k]] )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2,
	main = paste0("alpha = ",a[k], ", beta = ",b[k]) )
k=4; plot(cumsum(Xs[[k]] )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2,
	main = paste0("alpha = ",a[k], ", beta = ",b[k]) )

lapply( Xs, geweke.diag ) 



acf(Xs[[j]][-(1:(B/2))])
mean(Ar[[j]])


#################### #####################################################

rm(list = ls())

fx <- function( Alpha, Beta, p, N  ) { 
	( gamma(Alpha + Beta) / gamma( Alpha ))*
	p^(Alpha ) * Alpha^(1-1)* exp(- Alpha * 1 )  }
 
gx <- function( Alpha, Beta, p, N ) { 
	( gamma(Alpha + Beta) / gamma( Beta))*
	(1-p)^(Beta ) * Beta^(1-1)* exp(- Beta * 1 )  }

N <- 1
B <- 10000*2
xs <- ar <- rep(0, B)
ar <- vector("numeric", B)
Beta <- rep(NA,B); Beta[1] <- 1
Alpha <- rep(NA,B); Alpha[1] <- 1
p <- rep(NA,B); p[1] <- .2
As <- Bs <- rep(NA,B)
x <- rep(NA,B);x[1] <- 1
 i = 2
Ar <- Br <- rep(0,B)

a1 <- 1
a2 <- 1

set.seed(1789)
for( i in 2:B){

	Astar	<- rgamma(1, .27  , 1  )
	rho <- (  	fx(Astar, Beta[i-1], p[i-1], N ) / 
			fx( Alpha[i-1] , Beta[i-1], p[i-1], N )  ) * 
		(	dgamma( Alpha[i-1] , .27, .73 ) / dgamma( Astar, .27, .73  ))
	rho <- min(1, rho )
	
	if( runif(1) < rho ){ Alpha[i] <-   Astar; Ar[i] <- 1 } else {
		Alpha[i] <-  Alpha[i-1] }
 

	Bstar	<- rgamma(1, 100-27, 100 )
	rho <- (  	gx(Beta[i-1], Bstar, p[i-1], N ) / 
			gx( Alpha[i-1] , Beta[i-1], p[i-1], N )  ) * 
		(dgamma( Beta[i-1], 100-27, 100 ) / dgamma( Bstar,100-27, 100))
	rho <- min(1, rho )
	
	if( runif(1) < rho ){ Beta[i] <-  Bstar; Br[i] <- 1 } else {
		Beta[i] <-  Beta[i-1]   }
 

	Bs[i] <- Beta[i]

	p[i] <- rbeta(1, x[i-1] + Alpha[i] , N + Beta[i] - x[i-1] )
 	x[i] <- rbinom(1,N,p[i])

	}
 
 
rbinom(100, 1, 0.27)

mean(x)
acf(tail( x , B/2 ))
acf(tail( p , B/2 ))

mean( Ar ) ; mean( Br )

plot(cumsum( tail( x , B/2 ) )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2 )

plot(cumsum( tail( Alpha , B/2 ) )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2 )
plot(cumsum( tail( Beta , B/2 ) )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2 )




table(x)
mean(tail( x , B/2 ))
acf(  )


rm(list = ls())
coup <- read.table("coupsd.txt",header = F); colnames(coup) <- "s"
head(coup)


# ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  







rm(list = ls())
coup <- read.table("coupsd.txt",header = F); colnames(coup) <- "s"
head(coup)
require(  "rmutil") 

B <- 20000

mu <- lambda <- s <- rep(NA, B )
mu[1] <- s[1] <- mean(coup$s)
lambda[1] <- (sd(coup$s))^(-1)
Ar <- rep(0,B)
pmu <- function(M, L, S ) { exp( (-L/(2*M^2)) * sum( ((S-M)^2)/S))}
n <- nrow(coup)
SD <- .1

for( i in 2:B){

	mustar <- rnorm( 1, mu[1] , SD )
	rho <- (  	pmu( mustar, lambda[i-1], coup$s ) / 
			pmu( mu[i-1] , lambda[i-1],coup$s  )  ) * 
		(	dnorm( mu[i-1] , mu[1] , SD ) / dnorm( mustar,mu[1] , SD ) )
	rho <- min(1, rho )
	
	if( runif(1) < rho ){ mu[i] <- mustar; Ar[i] <- 1 } else {
		mu[i] <-  mu[i-1] }
 
	lambda[i] <- rgamma( 1 , n/2, (1/(2*(mu[i]^2))) * 
		sum( ((coup$s-mu[i]))^2/coup$s ) )

	s[i] <- rinvgauss(  1 , mu[i]  , 1/lambda[i] )
}


mean(Ar)

 

plot(cumsum(  tail( mu , B/2 )    )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2 )


plot(cumsum(  tail( s , B/2 )    )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2 )

s <- tail(s,10000)
mu<- tail(mu,10000)
lambda<- tail(lambda,10000)
plot(cumsum(   ( lambda)    )/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2 )


acf(lambda[seq(1, length(lambda), 5) ])
acf(lambda[seq(1, length(lambda), 10)] )
acf( lambda[seq(1, length(lambda), 20)] )

acf(lambda[seq(1, length(mu), 5) ])
acf(lambda[seq(1, length(mu), 10)] )
acf( lambda[seq(1, length(mu), 20)] )

acf(lambda[seq(1, length(lambda), 5) ] , main = "lambda, thin by 5")



