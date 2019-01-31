library(mcmcplots)
require(ggplot2, quietly = T)
rm(list = ls())

credInt <- function( x ){
	densOut <- density(x)
	credint <- quantile(x, probs = c(0.025, 0.975))
	densOut <- density(x)
	idStart <- max(which(densOut$x < credint[1])) + 1
	idEnd	 <- min(which(densOut$x > credint[2])) - 1
	gx <- densOut$x[idStart:idEnd]
	gy <- densOut$y[idStart:idEnd]
	px <- rep(0, length(gy))
	return( polygon(c(gx, rev(gx)), c(px, rev(gy)), border = F, col 
		= rgb(0, 0, 1, alpha = 0.5))	) }


## 1A
X <- 165
n <- 716
  
alpha = X + 1 
Beta = n - X + 1

#mean
alpha / ( alpha + Beta   )

# Variance 
( alpha * Beta )  / (  ( alpha + Beta )^2 * ( alpha + Beta +1 ) )

# Mode
( alpha - 1 ) / ( alpha + Beta - 2 ) 

betaSamples	<- as.data.frame( matrix(rbeta(10000, alpha , Beta ), , 1) )

denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)
credInt(betaSamples[,1]) 

#dev.new()
#ggplot(betaSamples, aes(V1)) + geom_density( col="#fbb4ae" )



## Example 1B
Y <- 175
m <- 919


alpha = X + Y + 1 
Beta = m + n - ( X + Y ) + 1

#mean
alpha / ( alpha + Beta   )

# Variance 
( alpha * Beta )  / (  ( alpha + Beta )^2 * ( alpha + Beta + 1 ) )

# Mode
( alpha - 1 ) / ( alpha + Beta - 2 ) 

betaSamples	<- as.data.frame( matrix(rbeta(10000, alpha , Beta ), , 1) )

dev.new()
denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)
credInt(betaSamples[,1]) 




## Example 1B
Z <- 142
l <- 1289


alpha = X + Y + Z + 1 
Beta = l + m + n - ( X + Y + Z ) + 1

#mean
alpha / ( alpha + Beta   )

# Variance 
( alpha * Beta )  / (  ( alpha + Beta )^2 * ( alpha + Beta + 1 ) )

# Mode
( alpha - 1 ) / ( alpha + Beta - 2 ) 

betaSamples	<- as.data.frame( matrix(rbeta(10000, alpha , Beta ), , 1) )

dev.new()
denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)
credInt(betaSamples[,1]) 






