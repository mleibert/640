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
 

 



lines( density ( rgamma( Nsim , n*k, n *xxbar ) ) )

set.seed(2020)
#female
xx <- rnorm( Nsim , (n*k-1) / (n * xxbar ) , sqrt(n*k-1) / (n * xxbar ) )
quantile( xx  , probs = c(.5,.025,0.975) )
 

plot( density (  1 / xx ) , xlim = c(30,60) ) 
lines( density ( 1/ xy )  )

d <- data.frame( density(xx)[[1]], round(density(xx)[[2]],6) )

d <- data.frame( 	c(1/xx ,1/ xy ), c(  rep("M",Nsim) , rep("F",Nsim) )  )	 
tail(d)
colnames(d) = c("mf","Sex" )

ggplot(   d , aes(mf, fill = Sex, color = Sex)    ) + theme_minimal() +  
	 geom_density(alpha = 0.1)  + 
	scale_fill_manual(values = c("green","purple")) +
 	scale_color_manual(values = c("green","purple")) + 
  	xlab("Average time to developing cancer")  
 
qu <- quantile(1/xx, probs  = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(1/xx)[[1]], round(density(1/xx)[[2]],6) )
colnames(d) = c("xx" , "xxy" ) 
d$xxarea <- d[,1] > qu[1] & d[,1] < qu[2]

tail(d)

qu <- quantile(1/xy, probs  = c(0.5, 0.025, 0.975))[2:3]
f <- data.frame( density(1/xy)[[1]], round(density(1/xy)[[2]],6) )
 colnames(f) = c("xy" , "xyy" ) 
f$xyarea <- f[,1] > qu[1] & f[,1] < qu[2]
tail(f)
tail(d)

d <- cbind(d,f)


ggplot( data = d       ) + theme_minimal() +
	geom_line( aes(x  = xy , y =xyy   )  ,col="purple" )   +
	geom_line( aes(x  = xx , y =xxy   )    , col="green" ) +
	geom_ribbon(data = d[which(d$xyarea == T),], aes(xy, ymin=0, 
		ymax=xyy , fill="Male" ), alpha = .1) +
	geom_ribbon(data = d[which(d$xxarea == T),], aes(xx, ymin=0, 
		ymax=xxy, fill="Female  "), alpha = .1)  + 
	scale_fill_manual(values = c("green","purple")) +
	theme(legend.position="bottom",   legend.title = element_blank() ) + 
  	xlab("Average time to developing cancer") + ylab("")
	
 

ggplot()+
    geom_line(data=Summary,aes(y=Y1,x= X,colour="darkblue"),size=1 )+
    geom_line(data=Summary,aes(y=Y2,x= X,colour="red"),size=1) +
    scale_color_discrete(name = "Y series", labels = c("Y2", "Y1"))


########################################################################

rm(list = ls())
dat <- read.table("coup1980.txt",header = T)
require(MASS)
require(invgamma)
tail(dat)

n = nrow(dat)
y <- dat[,2]
X <- as.matrix( dat[, - c(1,2 ) ] )
X <- cbind(1,X) ;   colnames(X)[1] <- "(Intercept)"
p <- ncol(X)
BHat <- solve( t(X)%*%X ) %*% t(X) %*% y


solve( t(X)%*%X ) %*% (t(X) %*% X ) %*% as.matrix(y)


Nsim = 10000
set.seed(1980)
lambda <- rgamma(Nsim, (n-p)/ 2 , .5*t( y- X %*% BHat )%*% (y- X %*% BHat ))
#lambda <- rinvgamma(Nsim, (n-p)/ 2 -2 ,.5*t( y- X %*% BHat )%*%
	# (y- X %*% BHat ))

lambda[1] 
 
1.82 + 4.2

set.seed(1980)
Beta <- matrix(NA, Nsim , p )
for( i in 1:Nsim){ Beta[i,] <- mvrnorm( 1, BHat, (  1/lambda[i] ) * 
	solve( t(X)%*% X )    ) }

glm( logCoup~.   , data = dat[,-1 ] )
apply( Beta , 2 , mean) 
colnames(Beta) <- colnames(X)
round( t( apply(Beta, 2, quantile, probs = c(0.5, 0.025, 0.975)) ) , 4 )


max( exp( predict( glm( logCoup~.   , data = dat[,-1 ] )  ) ) )*100

exp( t(apply( Beta , 2 , mean)) %*% c( 1 ,  1 , 72 , 25 )   )
exp( t(apply( Beta , 2 , mean)) %*% c( 1 ,  0 , 90 , 30*12)   )

 


