rm(list = ls())
setwd("G:\\math\\640")
options(scipen=999)
require(VGAM); require(invgamma)
 library(mvtnorm)
source("multiplot.R")
wind <- read.table("wind.txt",header = F)
n <- nrow(wind)

 

#plot( density( 1/rgamma( 111113 , n , (1/2)* sum(wind[,1]^2) ) ) )
#curve(dinvgamma(x , n  , (1/2)* sum(wind[,1]^2)   ) , 1 ,100 ,add=T )

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

ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="red"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="red", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()
 
rmean <- theta * sqrt(  pi / 2 )
summary(rmean )

summary(rmean )
denplot(rmean,   main = "Empirical Posterior Density Mode", lwd = 2)
quantile(rmean, probs = c(0.025, 0.5, 0.975))

qu <- quantile(rmean, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(rmean)[[1]], round(density(rmean)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="blue"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="blue", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()

rmedian <- theta * sqrt(  2 * log(2) )
summary(rmedian )

summary(rmedian )
denplot(rmedian,   main = "Empirical Posterior Density Mode", lwd = 2)
quantile(rmedian, probs = c(0.025, 0.5, 0.975))

qu <- quantile(rmedian, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(rmedian)[[1]], round(density(rmedian)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="green"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="green", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()+
   ggtitle("Sampled Posterior Density")

########

PPD <- thetatheta *0
set.seed(17)
for( i in 1:Nsim){  PPD[i] <- rrayleigh(1,  sqrt(thetatheta[i])) }

qu <- quantile(PPD, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(PPD)[[1]], round(density(PPD)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="purple"  ) +
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="purple", alpha = .15) +
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ theme_minimal()+
   ggtitle("Sampled Posterior Density")
 
 
sum(1*(PPD > 15))/length(PPD)


########################################################################

cb <- read.table("day.txt",header = T)
tail(cb )

dev.new(width=8-(.75*2), height=11-(.75*2))
 par(mfrow=c(4,2) ,  mar=c(2.1,4.1,2.1,2.1) )
ty <-  (  cb$casual ); plot(density(ty)); qqnorm(ty); qqline(ty )

ty <- scale(cb$casual) ; plot(density(ty)); qqnorm(ty); qqline(ty )

ty <- sqrt(  cb$casual ); plot(density(ty)); qqnorm(ty); qqline(ty )

ty <- log(  cb$casual ); plot(density(ty)); qqnorm(ty); qqline(ty )



 
n <- nrow(cb )
Nsim <- 20000
y1 <- sqrt( cb[,1] )	#casual
X <- as.matrix( cbind(1, cb[,-(1:2)] ) )
colnames(X)	<- c("(Intercept)",colnames(cb[,-(1:2)] ))
p <- ncol(X)

bhat <-  (solve(t(X)%*%X)%*%(t(X)%*%y1))
fit <- ( lm( y1~X[,-1]  )); coef(fit)
SSY <- t(y1 - X%*%bhat)%*%(y1 - X%*%bhat); anova(fit)
#vcov( fit )
rbetas <- matrix(0, nrow = Nsim, ncol = p)
XtXi	<- solve(t(X)%*%X)

set.seed(13)
rsig	<- rinvgamma(Nsim , (n-p)/2, (1/2)*SSY)
for(i in 1:Nsim){
	CovX	<- rsig[i]*XtXi
	rbetas[i,] <- c(rmvnorm(1, mean = bhat, sigma = CovX ) )
}

rbMat	<- apply(rbetas, 2, quantile, probs = c(0.5, 0.025, 0.975))
colnames(rbMat)	<- colnames(X)
coef(fit)
rbMat <- as.data.frame( round(t(rbMat), 4) )

rbMat$zero <- rbMat[,2] < 0 & rbMat[,3] > 0

hist(cb[,1])



rbsum <- ( apply( rbetas , 2 , summary) )
colnames(rbsum )	<- colnames(X)
 rbsum <- t( rbsum )

require(RColorBrewer)


 
colnames(rbetas)	<- colnames(X)

plotz <- list()
for( j in 1:ncol(X)) {
	Tdat <- rbetas[,j]
qu <- quantile(Tdat, probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(Tdat)[[1]], round(density(Tdat)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]
J <- ifelse( j == 2 , 9,j )
COLR <- brewer.pal(9,"Set3")[J]
 

 plotz[[j]] <- ggplot( data = d , aes(x=x , y=y)    ) + 
		geom_line( col=COLR  ) + theme_minimal()+
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ),
		 fill=COLR , alpha = .15) + ylab("") +
	theme(legend.position="none", axis.title.x=element_blank(), 
		axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ 
   	ggtitle( colnames(rbetas)[j] ) 
 }


multiplot(plotz[[1]], plotz[[2]], plotz[[3]], plotz[[4]], 
plotz[[5]], plotz[[6]], plotz[[7]], plotz[[8]], cols=2)

 brewer.pal(9,"Set3")







####

yrplots <- list()
yrsums <- list()
dim(apply(rbetas, 1 , function(B) (X%*%B)^2))
Yc <- apply(rbetas, 1 , function(B) (X%*%B)^2)
DATES <-  seq( as.Date("2011-01-01"), as.Date("2012-12-31"), by="+1 day")
DATES <-  DATES[-which( DATES  == "2012-02-29") ]
format(DATES , format = "%y/%q")
library(zoo)
rownames(Yc) <- gsub(" ","",as.character(as.yearqtr(DATES)))

 

for( i in 1:length( unique(rownames(Yc)) ) ){
 	
	yq <- unique(rownames(Yc))[i]
 	yq <-  Yc[ which( rownames(Yc) == yq ) , ] 
	yq <-  colSums(yq) / dim(yq)[1] 
	ycsums[[i]] <- summary(yq)
	
	qu <- quantile(yq, probs = c(0.5, 0.025, 0.975))[2:3]
	d <- data.frame( density(yq)[[1]], round(density(yq)[[2]],6) )
	colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]
	J <- ifelse( i == 6 , 9,i ); COLR <- brewer.pal(9,"Set3")[J]
	COLR <- brewer.pal(9,"Set1")[J]
 

 plots[[i]] <- ggplot( data = d , aes(x=x , y=y)    ) + 
		geom_line( col=COLR  ) + theme_minimal()+
	geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ),
		 fill=COLR , alpha = .15) + ylab("") +
	theme(legend.position="none", axis.title.x=element_blank(), 
		axis.title.y=element_blank(),
	     plot.title = element_text(hjust = 0.5)   )+ 
   	ggtitle(  unique(rownames(Yc))[i] ) 
 }


multiplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
plots[[5]], plots[[6]], plots[[7]], plots[[8]], cols=2)

ysums <- as.matrix(do.call(rbind, ycsums))
ysums <- as.data.frame(t(ysums))
colnames(ysums) <- paste0("Yc",unique(rownames(Yc)))

 
pred <- c(1,1,0,1,.344348,0.34847,0.804783,0.179117)
length(pred)
( pred %*% rbetas[1, ])^2

rrr <- apply( yrbetas , 1 , function(B) pred %*% B)

rsig	<- rinvgamma(Nsim , (n-p)/2, (1/2)*SSY)

mat <- matrix(NA,20000,730)

dim(rbetas )

pred%*%rbetas[1,]
 apply( ycrbetas , 1 , function(B) (pred %*% B)^2 )


