

fx <- function(x){ ifelse( x < 0 , 0,
	ifelse( 0 <= x & x < 0.5 ,  4*x ,
	ifelse( 0.5 <= x & x <= 1, 4-4*x,  
	ifelse(  x < 1, 0, 0 )
)))}


### U(0,1) ###
curve(fx, from = 0-.05, to = 1+.05)
M <- 2
curve(M *dunif(x, 0, 1), add=TRUE, col="gold", lwd=2)

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

 
