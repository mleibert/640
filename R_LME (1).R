## random intercept model ##
library(SemiPar)
library(MCMCpack)
library(mvtnorm)
data(pig.weights)
head(pig.weights)


Y	<- pig.weights$weight
N	<- length(Y)
n	<- length(unique(pig.weights$id.num))
X	<- cbind(rep(1, N), pig.weights$num.weeks)

B	<- 10000

beta	<- matrix(0, nrow = B, ncol = 2)
U		<- matrix(0, nrow = B, ncol = n)
taue	<- vector('numeric', length = B)
taub	<- vector('numeric', length = B)

xtx		<- solve(t(X)%*%X)

beta[1,]	<- xtx%*%t(X)%*%Y
U[1,]		<- rep(1, n)
taue[1]		<- 1
taub[1]		<- 1

Z	<- matrix(0, nrow = N, ncol = n)
for(i in 1:ncol(Z)){
	ni	<- length(which(pig.weights$id.num == i))
	Z[((i-1)*ni+1):(ni*i), i]	<- rep(1, ni)
}

ztz	<- t(Z)%*%Z
Inn	<- diag(n)

set.seed(1895)
for(t in 2:B){
	## update beta ##
	vbeta	<- (1/taue[t-1])*xtx
	mbeta	<- xtx%*%t(X)%*%(Y - Z%*%U[t-1,])
	beta[t,]	<- rmvnorm(1, mbeta, vbeta)
	
	## update U ##
	# vU		<- taue[t-1]*solve(taue[t-1]*ztz + taub[t-1]*Inn) # equals taue[t-1]/(taue[t-1]*ni + taub[t-1])*Inn
	# mU		<- vU%*%t(Z)%*%(Y-X%*%beta[t-1,])
	# U[t,]	<- rmvnorm(1, mU, vU) # can be slow for large n
	
	vU		<- taue[t-1]/(taue[t-1]*ni + taub[t-1])
	mU		<- vU*t(Z)%*%(Y-X%*%beta[t-1,])
	U[t,]	<- rnorm(n, mU, vU) # faster than rmvnorm, since vU is same for all subjects
	
	## update taue ##
	qfe		<- t(Y - X%*%beta[t-1,] - Z%*%U[t-1,])%*%(Y - X%*%beta[t-1,] - Z%*%U[t-1,])
	taue[t]	<- rgamma(1, N/2, (1/2)*qfe)
	
	## update taub ##
	qfb		<- t(U[t-1,])%*%U[t-1,]
	taub[t]	<- rgamma(1, n/2, (1/2)*qfb)
}

betab	<- beta[-c(1:(B/2)),]
apply(betab, 2, quantile, probs = c(0.025, 0.5, 0.975))

