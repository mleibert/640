### Example II.R7 ###
library(MCMCpack)
hers	<- read.table('hersct.txt', header = TRUE)
head(hers)

hercl	<- hers[-which(apply(is.na(hers), 1, sum) > 0),]

hp		<- hercl[which(hercl$treatment == 'placebo'),]
hht	<- hercl[which(hercl$treatment == 'hormone therapy'),]
B		<- 10000
np		<- nrow(hp)
yp		<- hp$chtchol
mup	<- mean(yp)

nh		<- nrow(hht)
yh		<- hht$chtchol
muh	<- mean(yh)

sigmas		<- matrix(1, nrow = B, ncol = 2)
mus			<- matrix(0, nrow = B, ncol = 2)
mus[1,]		<- c(mean(yp), mean(yh))
sigmas[1,]	<- c(var(hp$chtchol), var(hht$chtchol))

set.seed(1222)
for(t in 2:B){
	## update sigmas ##
	sigmas[t,1] <- rinvgamma(1, np/2, (1/2)*sum((yp - mus[t-1,1])^2))
	sigmas[t,2] <- rinvgamma(1, nh/2, (1/2)*sum((yh - mus[t-1,2])^2))
 
	## update mus ##
	mus[t,1] <- rnorm(1, mup, sqrt(sigmas[t-1,1]/np))
	mus[t,2] <- rnorm(1, muh, sqrt(sigmas[t-1,2]/nh))
}

geweke.diag(sigmas)
geweke.diag(mus)

t(apply(sigmas, 2, quantile, probs = c(0.5, 0.025, 0.975)))

diff	<- mus[,1] - mus[,2]

quantile(diff, probs = c(0.5, 0.025, 0.975))


### Example II.R8 ###
library(mvtnorm)
library(MCMCpack)
library(mcmcplots)

hers	<- read.table('hersreg.txt', header = TRUE)

set.seed(50)

B	<- 10000
k	<- dim(hers)[2]-1
n	<- dim(hers)[1]
y	<- hers[,1]
X	<- as.matrix(cbind(rep(1, n), hers[,c('treatment', 'sbp', 'statins')]))
colnames(X) <- c('intercept', colnames(X)[2:4])

sig		<- rep(0,B)
betamat	<- matrix(0, nrow = B, ncol = k)
bhat		<- c(solve(t(X)%*%X)%*%(t(X)%*%y))
v			<- solve(t(X)%*%X) 
m			<- v%*%(t(X)%*%y)  # more generally, diag(t0mat)%*%m0

sig[1]			<- (1/(n-k))*sum((y-X%*%bhat)^2)
betamat[1,]	<- bhat

for (t in 2:B) { 
	# Sample Beta #
	betamat[t,] <- c(rmvnorm(1, m, sig[t-1]*v)) 

	# Sample Sigma^2 #
	sig[t] <- rinvgamma(1, n/2, sum((y-X%*%betamat[t-1,])^2)/2)
}
colnames(betamat)	<- colnames(X)

betai	<- betamat[(B/2+1):B,]
sigi		<- sig[(B/2+1):B]

t(apply(betai, 2, quantile, probs = c(0.5, 0.025, 0.975)))
rmeanplot(betai)
geweke.diag(betai)
caterplot(betai, denstrip = TRUE)
mean(sigi)

sigMat				<- matrix(sigi, ncol = 1)
colnames(sigMat)	<- 'sigma^2'
mcmcplot1(sigMat, greek = TRUE)









