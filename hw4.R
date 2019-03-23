7

setwd("G:\\math\\640")
B <- 10000
ff <- read.table("forestfire.txt", header=TRUE)

Beta <- log( mean(ff[,1]) / (1-mean(ff[,1])) )
Beta

info <- 1/ ( mean(ff[,1])*( 1 - mean(ff[,1]) )*nrow(ff) )

set.seed(821); r.beta <- rnorm(B, mean=Beta, sd= sqrt(info) )


rmuList <- mcmc.list(list(mcmc(r.beta)))
mat <- matrix(r.beta , ncol = 1); colnames( mat ) <- "beta"
traplot( mat , greek = T )
rmeanplot(mat , greek = T)
autplot1( rmuList )
geweke.diag( r.beta )


n <- nrow(ff)
ybar <- mean(ff[,1])

BJeff <- log( ( n*ybar + .5 )  /  ( n + .5 - n*ybar )  )
IJeff <-  (n+1) * ( ( exp(BJeff) ) /  ( 1 + exp(BJeff) )^2 )
1/IJeff
set.seed(821); rj.beta <- rnorm(B , mean=BJeff , sd= 1/sqrt(IJeff ) )
 
rmuListj <- mcmc.list(list(mcmc(rj.beta)))
matj <- matrix(rj.beta , ncol = 1); colnames( matj ) <- "beta"
traplot( matj , greek = T )
rmeanplot(matj , greek = T)
autplot1( rmuListj )
geweke.diag( rj.beta )
mcmcplot1(mat, greek = T)



##########

bike <- read.table("bikeshare.txt", header=T)
head(bike)

fit <- glm( registered~ . -casual   , data = bike , family = poisson )
bhat	<- coef(fit)
vbeta	<- vcov(fit)
B <- 10000

set.seed(1959)
Beta <- rmvnorm(B, mean = bhat, sigma = vbeta)

t(apply(Beta, 2, quantile, probs = c(0.5, 0.025, 0.975)))
exp(t(apply(Beta, 2, quantile, probs = c(0.5, 0.025, 0.975))))

mat <- as.matrix(Beta   ) 
colnames( Beta ) <-  names( coef(fit) )
rmuList <- mcmc.list(list(mcmc(mat )))
traplot( mat , greek = T )
rmeanplot(mat , greek = T)
par(mfrow = c(2,4)); autplot1( rmuList )
geweke.diag( Beta  )


