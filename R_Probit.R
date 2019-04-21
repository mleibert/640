## probit regression ##
library(msm)
library(SemiPar)
library(mvtnorm)
data(trade.union)

Y	<- trade.union$union.member
n	<- length(Y)
X	<- cbind(trade.union$years.educ, trade.union$years.experience, trade.union$female)
n1	<- sum(Y)
n0	<- n - n1

B	<- 10000

beta0	<-	rep(0, ncol(X))				# Prior Mean for Beta
vbeta0	<-	diag(10, ncol(X))			# Prior Cov of Beta (vague)

# prec0	<- solve(vbeta0) # for conjugate prior
# vbeta	<- solve(prec0 + crossprod(X,X)) # for conjugate prior
vbeta	<- solve(crossprod(X,X))

beta	<- matrix(0, nrow = B, ncol = ncol(X))
z		<-rep(0,n)

set.seed(1262)
for(t in 2:B){
	## update beta ##
	bmean		<- vbeta%*%crossprod(X,z)
	# bmean		<- vbeta%*%(prec0%*%beta0 + crossprod(X,z)) # for conjugate prior
	beta[t,]	<- rmvnorm(1, bmean, vbeta)
	
	## update zi's ##
	xb			<- X%*%beta[t-1,]
	
	# yi = 0 #
	z[which(Y == 0)]	<- rtnorm(n0, mean = xb[which(Y == 0)], sd = 1, upper = 0)

	# yi = 0 #
	z[which(Y == 1)]	<- rtnorm(n1, mean = xb[which(Y == 1)], sd = 1, lower = 0)
	
}

