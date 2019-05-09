require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(MCMCglmm )
require(mvtnorm)
require(beepr)
require(mcmcplots)
require("Zelig")
require(statmod)
require(MCMCpack)
rm(list=ls());gc()

# onp <- read.csv("G:\\math\\640\\onp_test.csv")
head(onp)

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")

   
newdat <- data.frame(
  pared = rep(0:1, 200),
  public = rep(0:1, each = 200),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))


i = 1 
B <- 100000
X <- as.matrix( dat[,-1] )
 
Y <- as.factor(as.numeric(dat[,1]))
uy <- length( unique(Y) )
polr(Y ~ 1,  Hess=TRUE , method = "probit")
fit <- polr(Y ~ X   ,  Hess=TRUE , method = "probit")
fit1 <- polr(Y ~ 1   ,  Hess=TRUE , method = "probit")
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE , method = "probit")
X <- model.matrix( fit )
str(X)

gammas <- matrix( NA, B, uy + 1 )
gammas[,1] <- -Inf
gammas[,2] <- 0
gammas[,uy+1] <- Inf 
gammas[1,] <- c(-Inf, 0, fit$zeta[-1], Inf)

betas <- matrix( NA, B, length(coef(fit)) )
betas[1,] <- c(  coef(fit) )

#
betas <- matrix( NA, B, length(coef(fit)) + 1)
betas[1,] <- c( fit1$zeta[2], coef(fit) )

Z <-matrix(NA, B , nrow(X))
z <- Z[i,]
xtb <- X%*%betas[i,]
xtxi <- solve(t(X)%*%X)
Hat <- xtxi%*% t(X)

for( j in 1:uy ){
  z[Y==j] <- rtnorm( length(z[Y==j]) , xtb[Y==j] , 1,
                     lower = gammas[i, j], upper = gammas[i, j+1]  )
}
Z[i,] <- z

system.time(
  for( i in 2:B){
    
    for(j in 3:uy){
      lb <- max( max(Z[i-1,][Y==(j-1)])  , gammas[i-1,j-1]  )
      ub <- min( min(Z[i-1,][Y==(j)])  , gammas[i-1,j+1]  )
      gammas[i,j] <- runif(1, lb, ub )
    }
    gammas[i, ]
    
    z <- Z[i,]
    xtb <- X%*%betas[i-1,]
    
    for( j in 1:uy ){
      z[Y==j] <- rtnorm( length(z[Y==j]) , xtb[Y==j] , 1,
                         lower = gammas[i, j], upper = gammas[i, j+1]  )
    }
    Z[i,] <- z
    
    betas[i,] <- rmvnorm( 1 , Hat %*% Z[i ,] , xtxi  )
  }
)
beep("coin")

head(betas)
tail(betas  )

gammas <- tail(gammas,B/2)
betas <- tail(betas, B/2 )
mcmcplot1(betas[,3,drop  = F])
mcmcplot1(gammas[,3,drop=F])

Gammas <- apply( tail(gammas, B/2 ) , 2, median)
Betas <- apply( tail(betas, B/2 ) , 2, median)

coef(fit)
apply((betas),2,median)
pnewdat <- cbind(newdat, predict(fit, newdat, type = "probs"))
head(pnewdat)

lfit <-  polr(Y ~ X,  Hess=TRUE )
head( predict(m, newdat, type = "probs")  )
# head( predict(fit, Z , type = "probs")  )


ndm <- newdat
ndm$int <- 1
ndm <- ndm[,c(4,1:3)]
ndm <- as.matrix(ndm)

head( pnorm(  Gammas[2] -   ndm %*% Betas )  )
head( pnorm(  Gammas[3] -   ndm %*% Betas )  ) -  head( pnorm(  Gammas[2] -   ndm %*% Betas )  )
1 - head( pnorm(  Gammas[3] -   ndm %*% Betas )  ) 



 pnorm(  Gammas[2] -   as.matrix(newdat[1:5 ,1:3]) %*% apply(betas, 2, median) )  
1-pnorm(  Gammas[3] -   as.matrix(newdat[1:5,1:3]) %*% apply(betas, 2, median) )  











Betas - c(-1.297410, 0.597764 , 0.009265, 0.358459   ) 




z.out <- zelig(  as.numeric(apply) ~ pared + public + gpa, model = "oprobit.bayes", data = dat)
x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)
Betas





head( pnorm( 0 -  ndm   %*% c(-1.297410, 0.597764 , 0.009265, 0.358459   ) ) )

head( pnorm(  1.208190 -  ndm   %*% c(-1.297410, 0.597764 , 0.009265, 0.358459   ) ) )-
head( pnorm(  0 -  ndm   %*% c(-1.297410, 0.597764 , 0.009265, 0.358459   ) ) )
head(pnewdat)

 
mcmcfit <- MCMCoprobit( as.numeric(apply) ~ pared + public + gpa, data = dat , burnin =0, mcmc = B/2 )

apply(mcmcfit , 2 , median) 
head(mcmcfit)
head()

############################################################################################



rm(list=ls());gc()
setwd("G:\\math\\640")
source("onpdata.R"); beep("coin")

# 
# 
# dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
# 
# newdat <- data.frame(
#   pared = rep(0:1, 200),
#   public = rep(0:1, each = 200),
#   gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))
#  X <- as.matrix( dat[,-1] )
# Y <- as.factor(as.numeric(dat[,1]))


i = 1 
B <- 4000*20

# polr(Y ~ 1,  Hess=TRUE , method = "probit")
# fit <- polr(Y ~ X   ,  Hess=TRUE , method = "probit")
# fit1 <- polr(Y ~ 1   ,  Hess=TRUE , method = "probit")
# m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE , method = "probit")
# X <- model.matrix( fit )

fit <- onpfit
gammas <- matrix( NA, B, uy + 1 )
gammas[,1] <- -Inf
gammas[,2] <- 0
gammas[,uy+1] <- Inf
gammas[1,] <- c(-Inf, 0, fit$zeta[-1], Inf)

gammas[1,] <- c(-Inf, 0, .7,1.2,1.8, Inf)

betas <- alphas <- matrix( NA, B, length(coef(fit)) + 1)
p <- ncol(betas)
betas[1,] <- c( fit$zeta[1], coef(fit) )


xtb <- X%*%betas[i,]
xtxi <- solve(t(X)%*%X)
Hat <- xtxi%*% t(X)
xtx <- t(X)%*%X
 
alphas[1,] <- rinvgauss(p)
lambdas <- rep(NA,B)
a = b = 1
lambdas[i] <- sqrt( rgamma(1,a +  p/2 + 2 , b + (1/8) * sum(alphas[i,]*betas[i,]^2)  ) )

z <- Y*0

 for( j in 1:uy ){
  z[Y==j] <- rtnorm( length(z[Y==j]) , xtb[Y==j] , 1,
                     lower = gammas[i, j], upper = gammas[i, j+1]  )
}

system.time(
  for( i in 2:B){
    
    # Sample Gammas
    for(j in 3:uy){
      lb <- max( max(z[Y==(j-1)])  , gammas[i-1,j-1]  )
      ub <- min( min(z[Y==(j)])  , gammas[i-1,j+1]  )
      gammas[i,j] <- runif(1, lb, ub )
    }
    gammas[i, ]
    
    # Sample Zs
    xtb <- X%*%betas[i-1,]
    
    for( j in 1:uy ){
      z[Y==j] <- rtnorm( length(z[Y==j]) , xtb[Y==j] , 1,
                         lower = gammas[i, j], upper = gammas[i, j+1]  )
    }

        
    # sample lambda
    lambdas[i] <- sqrt( rgamma(1,a +  p/2 + 2 , b + (1/8) * sum(alphas[i-1,]*betas[i-1,]^2)  ) )
    
    #sample alphas
    s <- lambdas[i]^(-1)
    for(k in 1:p){
      alphas[i,k] <- rinvgauss(1 , (2*s)/abs(betas[i-1,k])  , 1 )
    }
    # alphas[i,]
    
    A <- ((lambdas[i])^2/4)  * diag(alphas[i,] )
    pxtx <- solve(xtx + A)
    betas[i,] <- rmvnorm( 1 , pxtx %*%  t(X)%*% z , pxtx )
    if( i %in% seq( 1000, 79000, 1000) ){print(i)}
  }
)
beep("coin")

write.csv(betas,"L1betas.csv",row.names=F)

gammas <- tail(gammas,B/2)
betas <- tail(betas, B/2 )
mcmcplot1(betas[,4,drop  = F])
mcmcplot1(gammas[,4,drop=F])

G <- gammas[,4]
mcmcplot1( as.matrix(G[seq(1,length(G),20)]) )



Gammas <- apply( tail(gammas[,3], B/2 ) , 2, median)
Betas <- apply( tail(betas, B/2 ) , 2, median)

cbind( predict(onpfit, onp.test[1:5,], type = "probs")  , onp.test[1:5,]$share_cat)
mcmcfit <- MCMCoprobit( share_cat ~ . , data = onp.train    )


coef( fit )
Betas

mcmcfit <- MCMCoprobit( share_cat ~ . , data = onp.train  , burnin = 10000 , mcmc = 10000  )

mcmcplot1(mcmcfit[,ncol(mcmcfit),drop=F])
tail(gammas)
head(mcmcfit)
tail(mcmcfit)

SVs <- list()


SVs[[1]] <- apply(mcmcfit,2,median)
SVs[[2]] <- tail( SVs[[1]] , 3)
SVs[[1]] <- head( SVs[[1]] , -3)


onpfit