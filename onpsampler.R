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
library(msm)

setwd("G:\\math\\640")
onp <- read.csv("onp_train.csv",header = T)
#   head(onp)

 
onp <- onp[,c(5:44,47,ncol(onp) )]
fit <- polr( as.factor(share_cat) ~ . , data = onp  , method = "probit" )
X <- model.matrix(fit)
X <- X[,-1]
Y <- as.factor( onp[,ncol(onp)] )
B <- 1000 

# MLE Betas
betas <- matrix(NA, B , length(coef(fit)))
betas[1,] <- coef(fit)

# MLE Gammas
gammas <- matrix( NA, B, 4 ); gammas[,1] <- 0
gammas[1, 2:4 ] <- fit$zeta[-1]; gammas[1,  ]
  
# Z's
Z <- matrix(NA, B , nrow(X))

gam <- data.frame( z = Z[1,], Y )
gam$xtb <- apply( X , 1 , function(Q) t(Q ) %*%  betas[1,] )  #cant vectorize...
head(gam)

i = 1 

## Horribly slow

for(k in 1:nrow(gam)){
  
  if( gam[k,]$Y == 1 ){ arg1 = -Inf } else { arg1 = 
    gammas[i,  as.numeric(gam[k,]$Y) -1 ] }
  
  if( gam[k,]$Y == 5 ){ arg2 = Inf } else { arg2 = 
    gammas[i  ,   as.numeric(gam[k,]$Y) ] }
  
  gam[k,1] <- rtnorm( 1  , gam[k,3] , 1 , 
            lower =  arg1 , upper = arg2  )
}
Z[i,] <- gam$z


#gammas <- cbind( rep(10e-15, nrow(gammas) )  , gammas )
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
gam <- data.frame( z = Z[1,], Y )
gam$xtb <- apply( X , 1 , function(Q) t(Q ) %*%  betas[1,] )  #cant vectorize...
head(gam)

## Horribly slow

for(k in 1:nrow(gam)){
  
  if( gam[k,]$Y == 1 ){ arg1 = -Inf } else { arg1 = 
    gammas[i,  as.numeric(gam[k,]$Y) -1 ] }
  
  if( gam[k,]$Y == 5 ){ arg2 = Inf } else { arg2 = 
    gammas[i  ,   as.numeric(gam[k,]$Y) ] }
  
  gam[k,1] <- rtnorm( 1  , gam[k,3] , 1 , 
                      lower =  arg1 , upper = arg2  )
}
Z[i,] <- gam$z


#sample betas
betas[i,] <- rmvnorm(1,  xtxi %*% (t(X)%*%Z[i,]) , xtxi )
}
); beep("mario")






betas <- tail(betas, B/2)
gammas<- tail(gammas, B/2)
Z <- tail(Z, B/2)

plot(density(gammas[,2])); median(gammas[,2])
plot(density(gammas[,3])); median(gammas[,3])
plot(density(gammas[,4])); median(gammas[,4])

fit

