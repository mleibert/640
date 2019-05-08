rm(list=ls());gc(); 
setwd("G:\\math\\640")

reign <- read.table("coup1280.txt", stringsAsFactors = F , header =T)
tail(reign)
reign$z <- scale(reign[,2])

POST <- function( B , w ){  (B/gamma(1/B))^(length(w)) * exp(- sum(abs( w )^B) )  }
Post <- function(B){ POST(B,reign$z)}
post <- function(B){ return(sapply(B, Post )) }

optimize(post,lower=1 , upper = 3, maximum = T)
 
a =.3; b =  .15

curve(post  , from = 0.1, to = 3  )
curve(   dlnorm(x, .3, .15 ), from = 0, to = 4 , add = T)

# Multiply Posterior by a constant to get a better look
POST <- function( B , w ){ 10e52* (B/gamma(1/B))^(length(w)) * exp(- sum(abs( w )^B) )  }
Post <- function(B){ POST(B,reign$z)}
post <- function(B){ return(sapply(B, Post )) }
curve(post   , from = 0.1, to = 3, ylim = c(0,2), col = "mediumvioletred",lwd = 2  )
curve(   dlnorm(x, .3, .15 ), from = 0, to = 4 , add = T, col = "olivedrab4" ,lwd = 2)
legend("topright", legend=c("Posterior", "Log Normal"),
       col=c("mediumvioletred", "olivedrab4"), lty=1 , lwd = 2, cex=0.8, text.font=4 )


# Remove constant
POST <- function( B , w ){  (B/gamma(1/B))^(length(w)) * exp(- sum(abs( w )^B) )  }
Post <- function(B){ POST(B,reign$z)}
post <- function(B){ return(sapply(B, Post )) }

# Find M
q <- function(v){post(v) /   dlnorm(v, .3, .15  ) }
M <- optimize( q , lower=.2, upper = 3, maximum = T )$objective


curve(post, from = .5, to = 2.5 , col = "mediumvioletred",lwd = 2  )
curve( M* dlnorm(x, a, b ), add = T, col = "olivedrab4" ,lwd = 2)
legend("topright", legend=c("Posterior", "Log Normal"),
       col=c("mediumvioletred", "olivedrab4"), lty=1 , lwd = 2, cex=0.8, text.font=4 )

#zoom
curve(post, from = 1, to = 1.7 , col = "mediumvioletred",lwd = 2 )
curve( M*dlnorm(x, a, b ), add = T, col = "olivedrab4" ,lwd = 2)
legend("topright", legend=c("Posterior", "Log Normal"),
       col=c("mediumvioletred", "olivedrab4"), lty=1 , lwd = 2, cex=0.8, text.font=4 )

curve(post, from = .5, to = 2.5  )
curve( M* dlnorm(x, a, b ), add = T)

Beta <- rep(NA, 8000)
i <- 1
count <- 1

set.seed(23)
while(i < 8001){
  tb <- rlnorm(1,a,  b)
  U <- runif(1)
  r <- post(tb) / (M*dlnorm(tb,a,  b))
  if(U < r){
    Beta[i] <- tb
    i <- i + 1
    points( tb , M*U*dlnorm(tb,a,  b), col = "blue" , pch = 16) } else {
      points( (tb) , M*U*dlnorm(tb,a,  b), col = "red" , pch = 4) }
  count <- count + 1 }
8000/count


median(Beta)


#######

rm(list=ls());invisible(gc())
setwd("G:\\math\\640")
library(rmutil, quietly = T)
library(MCMCpack, quietly = T)
library(beepr)

reign <- read.table("coup1280.txt", stringsAsFactors = F , header =T)
B = 10000
mus <- sigmas <- rep(NA,B)
n <- nrow(reign)
alphas <- matrix(NA, B, n )
x <- reign$logCoup

mup <- function( Alpha, y ){ sum(y*Alpha)/sum(Alpha) }
vp <- function( Alpha, ss ){ (4*ss) / sum(Alpha)  }
thetap <- function( Mu, ss, y){ (2*sqrt(ss))/ abs(y-Mu) }
bp <- function( Mu, Alpha, y ){(1/8) * sum(Alpha * (y-Mu)^2 )}
  
a = b = 1

alphas[1,] <- rinvgamma(n, 1, .5 )
sigmas[1] <- rinvgamma(1, (n/2) + a, b + (1/8) )
mus[1] <- rnorm(1 , mup(alphas[1,],x) , sqrt( vp(alphas[1,],sigmas[1] ) )  )

for( i in 2:B){
  mus[i] <- rnorm( 1, mup(alphas[i-1,],x) , sqrt( vp(alphas[i-1,],sigmas[i-1] ) ) )
  sigmas[i] <- rinvgamma(1, (n/2) + a , b + bp(mus[i-1],alphas[i-1] , x)  )
  for(k in 1:n){ alphas[i,k] <- rinvgauss(1, thetap(mus[1],sigmas[1],x[k] ),  1) }
}
beep("coin")

head(mus)

plot(density(x))

library(ggplot2, quietly = T)
 
#acceptance rates
unlist( Ars )

# Theta 

require(mcmcplots)

mcmcplot1( matrix(mus , ncol = 1)  )
quantile(mus , probs = c(0.5, 0.025, 0.975))
geweke.diag(mcmc( mus ) )

mcmcplot1( matrix(sigmas , ncol = 1)  )
quantile(sigmas , probs = c(0.5, 0.025, 0.975))
geweke.diag(mcmc( sigmas ) )





quantile(mus, probs = c(0.025, 0.5, 0.975))

qu <- quantile(mus , probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(mus)[[1]], round(density(mus)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="gold4"  ) +
  geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="gold4", alpha = .15) +
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5)   )+ theme_minimal()


Lambdas <- unlist(Lambdas)
quantile(Lambdas, probs = c(0.025, 0.5, 0.975))

qu <- quantile(Lambdas , probs = c(0.5, 0.025, 0.975))[2:3]
d <- data.frame( density(Lambdas)[[1]], round(density(Lambdas)[[2]],6) )
colnames(d) = c("x" , "y" ); d$area <- d[,1] > qu[1] & d[,1] < qu[2]

ggplot( data = d , aes(x=x , y=y)    ) + geom_line( col="purple"  ) +
  geom_ribbon(data = d[which(d$area == T),], aes(x, ymin=0 , ymax=y ), fill="purple", alpha = .15) +
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5)   )+ theme_minimal()

```





# 
# ######################
# B 	<- 16000
# m <- 2
# mu <- Ar	 <- vector("numeric", B)
# mu[1] <- 2
# 
# set.seed(7321)
# for(i in 2:B){
#   
#   mustar	<- rlnorm(1, .3, .15 )
#   aprob 	<- min(1, (
#     post( mustar )/
#       post( mu[i-1] )
#   )/(
#     dlnorm(mustar,  .3, .15  )/
#       dlnorm(mu[i-1], .3, .15  )
#   ))
#   U <- runif(1)
#   if(U < aprob){ m <- mustar; Ar[i] <- 1 }
#   mu[i]	 <- m
#   
# }
# 
# mean(Ar)
# 
# median(tail(mu,B/2))
# acf(mu)
# plot(cumsum(mu[ (1:(B ))])/(1:(B )), 
#      type = 'l', ylab = 'Running Mean', xlab = 'B', lwd = 1)
# 


##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################

rm(list=ls());gc(); 
# dev.off()
setwd("G:\\math\\640")
library(mcmcplots)
library(MCMCpack)

VA <- read.table('valc.txt', header = TRUE)
x <- VA$t
mean(x)

Thetas <- Lambdas <- Ars <- list()
thets <- c(0.1,0.2,0.15,0.05)
lams <- c( 1,3,0.5, 1.5)
seeds <- c(121,75,340,19)
a <- 4.2
b <- 3.9

thetaden <- function(y, theta, lambda ){ 
  lamthet <- lambda^theta
  ( theta / lamthet  )^n * 
    (prod(y))^theta * 
    exp( - (1/ lamthet) * sum(y^theta)  ) *
    (lamthet)^(-1)  
}

B <- 50000*2 

for( j in  1:4 ){

thetas <- lambdas <- rep(NA,B) 
Ar <- rep(0,B) 
thetas[1] <- th <- thets[j]
lambdas[1] <- lams[j]
n <- length(x)

set.seed(seeds[j])
for( i in 2:B){

  mu <- rinvgamma(1,  n ,  sum( x^thetas[i-1] ) )
  lambdas[i] <- mu^(1/thetas[i-1])
  
  thetastar <- rgamma(1, a, b)

  rho <- ( thetaden(x, thetastar, lambdas[i-1] ) / thetaden(x, thetas[i-1], lambdas[i-1] ) 
       ) / (
      dgamma(thetastar, a, b )/dgamma(thetas[i-1], a, b ) 
       )
  rho <- min(rho,1)
  
  U  <- runif(1)
  if( U < rho ){
    th <- thetastar
    Ar[i] <- 1
  }
  thetas[i]  <- th
}

Ars[[j]] <- mean(Ar)
Thetas[[j]] <- tail(thetas,B/2)
Lambdas[[j]] <- tail(lambdas,B/2)
}

c(j,i)
#######################################

par(mfrow=c(2,2) , mar=rep(2,4) )
for( w in 1:4){ acf(  Lambdas[[w]] ) }
par(mfrow=c(2,2))
for( w in 1:4){ acf(  Thetas[[w]] ) }


acf(  Thetas[[4]][ seq(1,B/2,5) ]  )

#
length( tail(thetas,B/2)[ seq(1,B/2,5) ] )


mcmcT <- mcmcL <- list()
for(w in 1:4){ 
  Lambdas[[w]] <- Lambdas[[w]][seq(1,B/2,5)]
  Thetas[[w]] <- Thetas[[w]][seq(1,B/2,5)]
  mcmcT[[w]] <- mcmc(Thetas[[w]])
  mcmcL[[w]] <- mcmc(Lambdas[[w]])
  }

GRtheta <- mcmc.list(list(mcmcT[[1]], mcmcT[[2]], mcmcT[[3]], mcmcT[[4]])) 
GRlambda <- mcmc.list(list(mcmcL[[1]], mcmcL[[2]], mcmcL[[3]], mcmcL[[4]])) 

gelman.diag(GRtheta)
gelman.diag(GRlambda)



length(Lambdas[[1]])
#




St <- function( y, theta, lambda  ){ exp( -y / lambda )^theta }
 
smat <- matrix(NA, max(x) , B/2 )

for( k in 1:(B/2) ){
  smat[,k] <- St( 1:max(x), thetas[k], lambdas[k] )
}

SCI <- matrix(NA, nrow(smat) , 2)

for( i in 1:nrow(smat)) {
  Q <- quantile(smat[i,], probs = c(0.025, 0.5, 0.975)) 
  SCI[i,] <- Q[c(1,3)]; rm(Q)
}

msf <- apply(smat, 1,median )
msf <- data.frame( time = 1:max(x) , msf )
msf$lower <- SCI[,1]
msf$upper <- SCI[,2]
plot(msf, type = "l")
points(SCI[,1],    type = "l")
points(SCI[,2],    type = "l")

p<-ggplot(data=msf, aes(x=time, y=msf ))  + geom_line( col = "blue")+ theme_minimal()

p+geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1, fill = "blue")


Thetas<- unlist(Thetas)
Lambdas<- unlist(Lambdas)

smat <- matrix(NA, max(x),length(Thetas )   )
dim(smat)
for( k in 1:length(Thetas ) ){
  smat[ ,k ] <- St( 1:max(x), Thetas[k], Lambdas[k] )
}


##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################

rm(list=ls());gc(); 
# dev.off()
setwd("G:\\math\\640")
library(mcmcplots)
library(MCMCpack)
library(mvtnorm)

B = 8000
wine <- read.table('vinhoverde.txt', header = TRUE)

fit  <- glm(quality ~ . ,   family = binomial, data = wine)
vbeta <- vcov(fit)
X <- model.matrix(fit) 
Y <- wine$quality


ssss	<- rmvnorm(4, mean = coef(fit), sigma = vbeta)


betas  <- matrix(0, nrow = B, ncol = ncol(X))
betas[1,] <- coef(fit)
Ar <- rep(NA,B-1)

tdens <- function(b, W, z){
 log( exp(sum(z*(W%*%b) - log(1 + exp(W%*%b)))) )
}

tdens <- function(b, W, z){
  sum( z*(W%*%b) ) - sum(log(  1 + exp(W%*%b))) 
}

tau = .3

set.seed(1908)
for(i in 2:B){
  
  bstar <- rmvnorm(1, betas[i-1,], tau*vbeta )
  r  <- exp(tdens(t(bstar), X, Y)-tdens(betas[i-1,], X, Y))
  U  <- runif(1)
  if(U < min(1,r)){
    betas[i,] <-bstar
    Ar[i-1]  <- 1
  } else{
    betas[i,] <- betas[i-1,]
    Ar[i-1]  <- 0
  }
  
}
mean(Ar)


betas <- tail(betas,B/2)


CI <- t(apply(betas, 2, quantile, probs = c(0.5, 0.025, 0.975)))
rownames(CI) <- colnames(X)
CI <- round(CI,6)
CI0 <- (CI[ ,2 ] < 0 & CI[ ,3] > 0 )
CI <-  CI[     !(CI[ 2,] < 0 & CI[ ,3] > 0 ), ] 
CI <- as.data.frame(CI)
CI$dif <- CI[,3]-CI[,2]

summary(fit)
a <- round( confint(fit) , 3)


acf(  betas[ seq(1,B/2,20) ,1]  )

nrow(betas[ seq(1,B/2,20) , ])*20
## THin


rm(list=ls());gc(); 

B = 8000 * 20
wine <- read.table('vinhoverde.txt', header = TRUE)

fit  <- glm(quality ~ . ,   family = binomial, data = wine)
vbeta <- vcov(fit)
X <- model.matrix(fit) 
Y <- wine$quality

betas  <- matrix(0, nrow = B, ncol = ncol(X))
betas[1,] <- coef(fit)
Ar <- rep(NA,B-1)

tdens <- function(b, W, z){
  log( exp(sum(z*(W%*%b) - log(1 + exp(W%*%b)))) )
}

tdens <- function(b, W, z){
  sum( z*(W%*%b) ) - sum(log(  1 + exp(W%*%b))) 
}

exp( tdens( betas[1,] , X, Y) )


tau = .3

set.seed(1908)
for(i in 2:B){
  
  bstar <- rmvnorm(1, betas[i-1,], tau*vbeta )
  r  <- exp(tdens(t(bstar), X, Y)-tdens(betas[i-1,], X, Y))
  U  <- runif(1)
  if(U < min(1,r)){
    betas[i,] <-bstar
    Ar[i-1]  <- 1
  } else{
    betas[i,] <- betas[i-1,]
    Ar[i-1]  <- 0
  }
  
}
mean(Ar)


betas <- tail(betas,B/2)
Bs <- betas[ seq(1,B/2,20) , ]

acf(  betas[,1]  )


par(mfrow=c(4,3))
for( k in 1:12){acf(Bs[,k])}


CI


summary( fit )


c("volatile.acidity" ,"citric.acid" ,    "chlorides", "free.sulfur.dioxide","density","pH", "alcohol")     





apply(betas)

CI <- t(apply(betas , 2, quantile, probs = c(0.5, 0.025, 0.975)))
rownames(CI) <- colnames(X)
CI <- round(CI,6)
CI0 <- (CI[ ,2 ] < 0 & CI[ ,3] > 0 )
CI <-  CI[     !CI0 , ] 
CI <- as.data.frame(CI)
CI$dif <- CI[,3]-CI[,2]
names( which( (summary(fit)$coeff[ ,4] < 0.05) == T) )   %in% rownames(CI)
  
  
wine[ ,which(colnames(wine)  %in% names( which( (summary(fit)$coeff[-1,4] < 0.05) == T) ) )]


# https://www.stat.cmu.edu/~cshalizi/uADA/12/lectures/ch12.pdf

bhat <- coef(fit)[ which( names( coef(fit) ) %in% rownames(CI) ) ]
vbetas <- vbeta[,which( names( coef(fit) ) %in% rownames(CI) ) ]
vbetas <- vbetas[ which( names( coef(fit) ) %in% rownames(CI) ), ]

set.seed(1908)
Betas	<- rmvnorm(B, mean = bhat, sigma = vbetas)



# log odds #
CIna <- t(apply(Betas, 2, quantile, probs = c(0.5, 0.025, 0.975)))
CIna <- round(CIna, 6)
CIna <- as.data.frame(CIna)
CIna$dif <- CIna[,3]-CIna[,2]

# odds #
oddsna <- exp(t(apply(Betas, 2, quantile, probs = c(0.5, 0.025, 0.975))))
oddsna <- round(oddsna, 5)


