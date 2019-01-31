require(ggplot2)

################################################################
################################################################

N = 1000
set.seed(108)
X <- as.data.frame( rgamma( N, 27, 3 ) )

plot( density( X[,1] ) );curve( dgamma(x, 27,3),  add=T, col="blue")

ggplot(X, aes(x=X[,1])) +   
	geom_density(  fill = "blue" , alpha = .1, col="blue")+ 
	stat_function(fun=dgamma,color="red",
	args=list(shape  = 27 ,  rate = 3 ), size = 1)


################################################################

N = 10000
set.seed(108)
X <- as.data.frame( rgamma( N, 27, 3 ) )

plot( density( X[,1] ) );
curve( dgamma(x, 27,3),  add=T, col="blue")

ggplot(X, aes(x=X[,1])) +   
	geom_density(  fill = "blue" , alpha = .1, col="blue")+ 
	stat_function(fun=dgamma,color="red",
	args=list(shape  = 27 ,  rate = 3 ), size = 1)


################################################################
################################################################


N = 1000
set.seed(108)
X <- as.data.frame( rbeta( N, .5, .5 ) )

plot( density( X[,1], from = 0 , to = 1 ) ); 
curve( dbeta(x, .5, .5 ),  add=T, col="blue")

ggplot(X, aes(x=X[,1])) +   xlim(-.25, 1.25) + ylim(0,5) +
	geom_density(  fill = "blue" , alpha = .1, col="blue", trim=T)  +  
	stat_function(fun=dbeta,color="red",
	args=list(shape1  = .5 ,  shape2 = .5 ), size = 1)

################################################################



N = 10000
set.seed(108)
X <- as.data.frame( rbeta( N, .5, .5 ) )

plot( density( X[,1], from = 0 , to = 1 ) ); 
curve( dbeta(x, .5, .5 ),  add=T, col="blue")

ggplot(X, aes(x=X[,1])) +   xlim(-.25, 1.25) + ylim(0,5) +
	geom_density(  fill = "blue" , alpha = .1, col="blue", trim=T)  +  
	stat_function(fun=dbeta,color="red",
	args=list(shape1  = .5 ,  shape2 = .5 ), size = 1)

################################################################
################################################################



N = 1000
set.seed(108)
X <- as.data.frame( rexp( N, 12 ) )

plot( density( X[,1], from = 0  )  , 	ylim=c(-0,   13 ) ); 
curve( dexp(x, 12 ),  add=T, col="blue")

ggplot(X, aes(x=X[,1])) +  #   xlim(-.1, 1.25) + ylim(0,5) +
	geom_density(  fill = "blue" , alpha = .1, col="blue", trim=T)  +  
	stat_function(fun=dexp,color="red",
	args=list( rate = 12 ), size = 1)















