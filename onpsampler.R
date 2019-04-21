rm(list=ls());gc()
require(MCMCpack)
require(mvtnorm)
require(MASS)
library(foreign) 

mydata <- read.dta("https://dss.princeton.edu/training/Panel101.dta") 

MCMCoprobit

x1 <- rnorm(100); x2 <- rnorm(100);
z <- 1.0 + x1*0.1 - x2*0.5 + rnorm(100);
y <- z; y[z < 0] <- 0; y[z >= 0 & z < 1] <- 1;
y[z >= 1 & z < 1.5] <- 2; y[z >= 1.5] <- 3;

out1 <- MCMCoprobit(y ~ x1 + x2, tune=0.3)
out2 <- MCMCoprobit(y ~ x1 + x2, tune=0.3, tdf=3, verbose=1000, mcmc.method="AC")
summary(out1)
summary(out2)
plot(out1)
plot(out2)

X <- as.matrix( mtcars[,-2] )
X <- as.matrix( mtcars[,c(1,3,4)] )
Y <- as.factor( mtcars[, 2] )
dat <- mtcars[,1:4]

polr( Y ~ rnorm(length(Y))    , Hess = T, method = "probit" )

polr( opinion ~ x1 + x2+x3 , data= mydata  )
str(mydata)
mydata$opinion

############

rm(list=ls());gc()
require(MCMCpack)
require(mvtnorm)
require(MASS)
library(foreign) 
require(beepr)

setwd("G:\\math\\640")
onp <- read.csv("onp_train.csv",header = T)
#   head(onp)

 
onp <- onp[,c(5:44,47,ncol(onp) )]
fit <- polr( as.factor(share_cat) ~ . , data = onp   )
X <- model.matrix(fit)
X <- X[,-1]
Y <- as.factor( onp[,ncol(onp)] )
B <- 1000 

betas <- matrix(NA, B , length(coef(fit)))
betas[1,] <- coef(fit)

Z <- matrix(NA, B , nrow(X))
xtb <- apply( X , 1 , function(Q) t(Q ) %*%  betas[1,] )
for( i in 1:nrow(X)){  Z[1,i] <- rnorm( 1 , xtb[i], 1 ) }


gammas <- matrix( NA, B, 4 ); gammas[,1] <- 0
gam <- data.frame( z = Z[1,], Y )
for( j in 2:4  ) { gammas[1,j] <- mean( gam[ which(gam[,2] == j) , 1] ) }

xtxi <- solve(t(X)%*%X, tol = 1e-18)
 

####

system.time(
for( i in 2:B) {

#sample gammas
gam <- data.frame( z = Z[i-1,], Y )
for( j in 2:4 ){
  arg1 <- max( gam[which(gam$Y == j),]$z )
  arg2 <- min( gam[which(gam$Y == (j+1)),]$z )
  arg3 <- c( max(max(arg1, gammas[i,1]  )) , min(min( arg2 , gammas[i-1,3]  )))
  gammas[i,j] <- runif( 1, min(arg3),max(arg3)) # gammas[i,] 
}
 
#sample Z's
xtb <- apply( X , 1 , function(Q) t(Q ) %*%   betas[i-1,] )
for( k in 1:nrow(X)){  Z[i,k] <- rnorm( 1 , xtb[k], 1 ) }

#sample betas
betas[i,] <- rmvnorm(1,  xtxi %*% (t(X)%*%Z[i,]) , xtxi )
}
); beep("coin")




