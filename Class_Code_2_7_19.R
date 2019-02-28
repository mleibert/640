### Example I.R8 ###
library(mvtnorm)
library(MCMCpack)
library(mcmcplots)

setwd('/Users/mjm556/Desktop/MATH 640')
hers	<- read.table('hersreg.txt', header = TRUE)

head(hers)

B	<- 10000
n	<- dim(hers)[1]
Y	<- hers[,1]
X	<- as.matrix(cbind(rep(1, n), hers[,c('treatment', 'sbp', 'statins')]))
colnames(X) <- c('intercept', colnames(X)[2:4])
K	<- dim(X)[2]

bhat		<- c(solve(t(X)%*%X)%*%(t(X)%*%y))
SSY		<- t(Y - X%*%bhat)%*%(Y - X%*%bhat)
XtXi		<- solve(t(X)%*%X)
rbeta	<- matrix(0, nrow = B, ncol = K)

set.seed(90210)
rsig	<- rinvgamma(B, (n-K)/2, (1/2)*SSY)
for(i in 1:B){
	CovX		<- rsig[i]*XtXi
	rbeta[i,]	<- c(rmvnorm(1, mean = bhat, sigma = CovX))
}

rbMat	<- apply(rbeta, 2, quantile, probs = c(0.5, 0.025, 0.975))
colnames(rbMat)	<- colnames(X)
t(rbMat)

round(t(rbMat), 4)

lm(Y ~ X - 1)


## Example I.R8 ##
lyme	<- read.table('lymeMN.txt', header = TRUE)
head(lyme)

y		<- lyme$count
B		<- 10000

## Normal Approx ##
mu		<- (sum(y)-1)/length(y)
sig2	<- (sum(y)-1)/(length(y)^2)

set.seed(217)
lamNA	<- rnorm(B, mean = mu, sd = sqrt(sig2))

## Gamma Posterior ##
alpha	<- sum(y)
beta		<- length(y)

set.seed(217)
lambda	<- rgamma(B, alpha, beta)


quantile(lambda, probs = c(0.025, 0.5, 0.975))
quantile(lamNA, probs = c(0.025, 0.5, 0.975))

plot(density(lambda), col = 'blue', lwd = 10, main = 'Posterior Comparison', ylab = expression(paste('p(', lambda,'|y)')), xlab = expression(lambda))
lines(density(lamNA), col = 'red', lwd = 10, lty = 3)
