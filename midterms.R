rm(list = ls())
setwd("G:\\math\\640")
options(scipen=999)
require(VGAM); require(invgamma)

wind <- read.table("wind.txt",header = F)
n <- nrow(wind)

 

plot( density( 1/rgamma( 111113 , n , (1/2)* sum(wind[,1]^2) ) ) )
curve(dinvgamma(x , n  , (1/2)* sum(wind[,1]^2)   ) , 1 ,100 ,add=T )

Nsim <- 2000
set.seed(17)
thetatheta <- rinvgamma(Nsim , n+1 , (1/2)* sum(wind[,1]^2)   ) 
quantile(thetatheta , probs = c(0.025, 0.5, 0.975))

#mode
theta <- sqrt( thetatheta  )
summary(theta)
denplot(theta,   main = "Empirical Posterior Density Mode", lwd = 2)
quantile(theta, probs = c(0.025, 0.5, 0.975))

qu <- quantile(theta , probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(theta)[[1]], round(density(theta)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

g1 <- ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="red"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="red", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()+
   ggtitle("Sampled Posterior Density")
g1

 
rmean <- theta * sqrt(  pi / 2 )
summary(rmean )

summary(rmean )
denplot(rmean,   main = "Empirical Posterior Density Mode", lwd = 2)
quantile(rmean, probs = c(0.025, 0.5, 0.975))

qu <- quantile(rmean, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(rmean)[[1]], round(density(rmean)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

g1 <- ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="blue"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="blue", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()+
   ggtitle("Sampled Posterior Density")
g1

rmedian <- theta * sqrt(  2 * log(2) )
summary(rmedian )

summary(rmedian )
denplot(rmedian,   main = "Empirical Posterior Density Mode", lwd = 2)
quantile(rmedian, probs = c(0.025, 0.5, 0.975))

qu <- quantile(rmedian, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(rmedian)[[1]], round(density(rmedian)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

g1 <- ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="green"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="green", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()+
   ggtitle("Sampled Posterior Density")
g1
########

PPD <- thetatheta *0
set.seed(17)
for( i in 1:Nsim){  PPD[i] <- rrayleigh(1,  sqrt(thetatheta[i])) }

qu <- quantile(PPD, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(PPD)[[1]], round(density(PPD)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

g1 <- ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="purple"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="purple", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()+
   ggtitle("Sampled Posterior Density")
g1
 
sum(1*(PPD > 15))/length(PPD)




dat <- read.table("day.txt",header = T)
tail(dat)

plot( density(   sqrt( dat$casual) ) )
plot( hist( log( dat$registered , base = 2) ))
plot( density(  ( dat$registered ) ))

n = nrow(dat)


 require(invgamma )
curve(dinvgamma(x,2,0) ,0 ,300000)



, from = 0, to = 1, 
xlab = expression(theta), 
ylab = expression(paste('p(',theta ,'|X)')),
 main = 'Posterior Density', lwd = 2, col = 'blue')
