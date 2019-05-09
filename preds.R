setwd("G:\\math\\640\\")
load("lsflatall.rdata")
source("onpdata.R"); beep("coin")

betas.flat.all <- ls.flat.all[['betas']]
gammas.flat.all <- ls.flat.all[['gammas']]

B <- 100000
betas.flat.all  <- tail( betas.flat.all  , B/2)
gammas.flat.all <- tail( gammas.flat.all , B/2)

betas.flat.all  <- betas.flat.all[seq(1,B/2,10),]
gammas.flat.all <- gammas.flat.all[seq(1,B/2,10),]


round( mcBetas - Betas , 3 )

# naive prediction

Betas <- apply( betas.flat.all   , 2, median)
Gammas <- apply( gammas.flat.all  , 2, median)
onps <- onp.test[,(ncol(onp.test)-1):ncol(onp.test)]

fit <- polr( as.factor(share_cat) ~ .   , data = onp.test, method = "probit" )
X <- model.matrix(fit) 
ncol(X)
pnorm( Gammas[5] -  X[4,] %*% Betas )


##############

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")

newdat <- data.frame(
  pared = rep(0:1, 200),
  public = rep(0:1, each = 200),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE , method = "probit")
pnewdat <- cbind(newdat, predict(m, newdat, type = "probs"))
head(pnewdat)

pnorm( m$zeta[1] - as.matrix( newdat[1,] ) %*%  coef(m) )
pnorm( m$zeta[2] - as.matrix( newdat[1,] ) %*%  coef(m) )-
  pnorm( m$zeta[1] - as.matrix( newdat[1,] ) %*%  coef(m) )
1 - pnorm( m$zeta[2] - as.matrix( newdat[1,] ) %*%  coef(m) )
##############
