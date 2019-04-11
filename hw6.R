require(MCMCpack)

Kumaraswamy <- function(x) { 2*2*x^(2-1)*(1-x^2)^(2-1) }

curve(Kumaraswamy(x) )

Kumaraswamy(.5)
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

	for( i in 2:B){
		xstar	<- rbeta(1, a[j], b[j])
		rho <- (  Kumaraswamy(xstar) / Kumaraswamy(x)  ) * 
			( dbeta( x , a[j], b[j] )  /  dbeta( xstar, a[j], b[j] ) )
		rho <- min(1, rho )
	
		if ( runif(1) < rho ){ x <- xstar; ar[i] <- 1}
		xs[i] <- x }
	Xs[[j]] <- xs
	Ar[[j]] <- ar
	}

j = 4
geweke.diag(Xs[[j]][-(1:(B/2))])
plot(cumsum(Xs[[j]][-(1:(B/2))])/(1:(B/2)), type = 'l', 
	ylab = 'Running Mean', xlab = 'B', lwd = 2)
acf(Xs[[j]][-(1:(B/2))])
mean(Ar[[j]])


#################### #####################################################

fx <- function( Alpha, Beta, p, N ) { 
	( gamma(Alpha + Beta) / gamma( Alpha ))*
	p^(Alpha-1) * Alpha^(1-1)* exp( Alpha * 1 )  }

gx <- function( Alpha, Beta, p, N ) { 
	( gamma(Alpha + Beta) / gamma( Beta))*
	(1-p)^(Beta-1) * Beta^(1-1)* exp( Beta * 1 )  }

N <- 100
B <- 10000*2
xs <- ar <- rep(0, B)
ar <- vector("numeric", B)
Beta <- rep(NA,B); Beta[1] <- 1
Alpha <- rep(NA,B); Alpha[1] <- 1
p <- rep(NA,B); p[1] <- .5
As <- Bs <- rep(NA,B)

for( i in 2:B){

	Astar	<- rgamma(1, 3, 3 )
	rho <- (  	fx(Astar, Beta[i-1], p[i-1], 100 ) / 
			fx( Alpha[i-1] , Beta[i-1], p[i-1], 100 )  ) * 
		(	dgamma( Alpha[i-1] , 3,3  )  /  dgamma( Astar,3, 3 ) )
	rho <- min(1, rho )
	
	if ( runif(1) < rho ){ Alpha[i] <- Astar }
	As[i] <-  Alpha[i]

	Bstar	<- rgamma(1, 3, 3 )
	rho <- (  	gx(Beta[i-1], Bstar, p[i-1], 100 ) / 
			gx( Alpha[i-1] , Beta[i-1], p[i-1], 100 )  ) * 
		(	dgamma( Beta[i-1] , 3,3  )  /  dgamma( Bstar,3, 3 ) )
	rho <- min(1, rho )
	
	if ( runif(1) < rho ){ Beta[i] <- Bstar }
	Bs[i] <- Beta[i]

	p[i] <- rbeta(1, 
 

	}



