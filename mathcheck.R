head(mtcars)

X <- as.matrix( mtcars[,c(1,3,4,5,6 )] )
z <- mtcars$qsec
rownames(X) <- colnames(X) <- NULL


fit <- lm( z ~ X )

b <- as.matrix( coef(fit) )[-1]

a <- rnorm(length(b))

lambda <-  11.49^4 / 4


exp( -.5 * t(z-X%*%b) %*% (z-X%*%b) - lambda * .5   * sum(a*b^2) )
exp( -.5 * (t(z)%*%z - t(z)%*%X%*%b - t(b)%*%t(X)%*%z + t(b)%*%t(X)%*%X%*%b 
	) - lambda *.5 * t(b) %*% (a*diag(length(b)) )%*% b )
exp( -.5 * (  
	t(z)%*%z + 
	t(b)%*%t(X)%*%X%*%b  + 
	( lambda * t(b) %*% (a*diag(length(b)) )%*% b)  - 
	( 2 * t(b)%*%t(X)%*%z  )
) )

exp( -.5 * (  
	t(z)%*%z + 
	t(b)%*% ( t(X)%*%X + lambda * (a*diag(length(b))) )  %*% b -
	( 2 * t(b)%*%t(X)%*%z  )
) )

exp( -.5 * (  
	t(z)%*%z + 
	t(b)%*% ( t(X)%*%X + lambda * (a*diag(length(b))) )  %*% b -
	( 2 * t(b) %*% ( t(X)%*%X + lambda * (a*diag(length(b))) ) %*%
	solve( t(X)%*%X + lambda * (a*diag(length(b))) )  %*% t(X)%*%z  )
) )







