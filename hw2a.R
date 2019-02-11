rm(list = ls())
setwd("G:\\math\\640")
require(mcmcplots, quietly = T)
require(ggplot2, quietly = T)

 

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


##  
X <- 165
n <- 716
  
alpha = X + 1 
Beta = n - X + 1

#mean
#alpha / ( alpha + Beta   )
# Variance 
#( alpha * Beta )  / (  ( alpha + Beta )^2 * ( alpha + Beta +1 ) )
# Mode
#( alpha - 1 ) / ( alpha + Beta - 2 ) 


bs  <- matrix( rbeta(10000, alpha , Beta ), , 1) 


# colnames(betaSamples) <- "x"
# brief discussion of the results



# posterior mean
mean( bs  )

# 95% credible interval
quantile(bs, probs = c(0.5, 0.025, 0.975))
qu <- quantile(bs, probs = c(0.5, 0.025, 0.975))[2:3]

# plot of the posterior distribution

#denplot(as.vector(bs[,1]), main = "Sampled Posterior Density", lwd = 2)
#credInt(bs[,1]) 

#dev.new()
d <- data.frame( density(bs)[[1]], round(density(bs)[[2]],6) )
colnames(d) = c("x" , "y" )
d$area <- d[,1] > qu[1] & d[,1] < qu[2]
 
 
 
ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="red" , size = 1) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , 
		ymax=y   ), fill="red", alpha = .2) +
	theme(legend.position="none") 



## Example 1B
Y <- 175
m <- 919


alpha = X + Y + 1 
Beta = m + n - ( X + Y ) + 1


bs	<-  rbeta(10000, alpha , Beta  , , 1) 

median(bs)

# 95% credible interval
quantile(bs, probs = c(0.5, 0.025, 0.975))
qu <- quantile(bs, probs = c(0.5, 0.025, 0.975))[2:3]

d <- data.frame( density(bs)[[1]], round(density(bs)[[2]],6) )
colnames(d) = c("x" , "y" )
d$area <- d[,1] > qu[1] & d[,1] < qu[2]
 
dev.new()
ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="blue" , size = 1) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , 
		ymax=y   ), fill="blue", alpha = .2) +
	theme(legend.position="none") +
 





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




######################################################################

skin <- read.table( "skin.txt"  , header = T)


bs <- rgamma( 10000, sum( skin[,1] ) + .5 ,  nrow(skin) )
qu <- quantile(bs, probs = c(0.5, 0.025, 0.975))[2:3]

d <- data.frame( density(bs)[[1]], round(density(bs)[[2]],6) )
colnames(d) = c("x" , "y" )
d$area <- d[,1] > qu[1] & d[,1] < qu[2]
 
ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="blue" , size = 1) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , 
		ymax=y   ), fill="blue", alpha = .2) +
	theme(legend.position="none") 
 






