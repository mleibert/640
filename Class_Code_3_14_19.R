### Example II.R2 ###
library(MCMCpack)
library(moments)

hers <- read.table('hers.txt', header = TRUE)
head(hers)

y		<- hers$x
ybar	<- mean(y)
sig		<- var(y)
n		<- length(y)
B		<- 10000

set.seed(3)

rmu		<- ybar + (sqrt(sig/n))*rt(B, df = n-1)
rsig	<- rinvgamma(B, (n-1)/2, ((n-1)/2)*sig)

library(coda)

geweke.diag(mcmc(rmu))

rmu1	<- mcmc(rmu)

set.seed(20057)
rmu2	<- mcmc(ybar + (sqrt(sig/n))*rt(B, df = n-1))

set.seed(3142)
rmu3	<- mcmc(ybar + (sqrt(sig/n))*rt(B, df = n-1))

set.seed(314)
rmu4	<- mcmc(ybar + (sqrt(sig/n))*rt(B, df = n-1))

allChains <- mcmc.list(list(rmu1, rmu2, rmu3, rmu4)) 

gelman.diag(allChains)

### Example II.R5 ###
# from Example II.R1 #
yobs	<- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

set.seed(1875)

B 		<- 10000
rtheta	<- rbeta(B, 8, 14)

n			<- length(yobs)
X			<- sum(yobs)
thb 		<- mean(rtheta)

binomLik	<- function(n,x,p){ choose(n, x)*(p^x)*(1-p)^(n-x)}

pDIC		<- 2*(log(binomLik(n, X, thb)) - (1/B)*sum(log(binomLik(n, X, rtheta))))
DIC		<- -2*log(binomLik(n, X, thb)) + 2*pDIC

# using built-in functions #

pDIC		<- 2*(log(dbinom(X, n, thb)) - (1/B)*sum(log(dbinom(X, n, rtheta))))
DIC		<- -2*log(dbinom(X, n, thb)) + 2*pDIC

# from Example II.R2 #
hers <- read.table('hers.txt', header = TRUE)

y		<- hers$x
ybar	<- mean(y)
sig	<- var(y)
n		<- length(y)
B		<- 10000

set.seed(3)

rmu		<- ybar + (sqrt(sig/n))*rt(B, df = n-1)
rsig		<- rinvgamma(B, (n-1)/2, ((n-1)/2)*sig)

lppdv		<- vector(length = n)
pwaicv		<- vector(length = n)

for(i in 1:n){
	yi			<- y[i]
	liki		<- dnorm(yi, rmu, sqrt(rsig)) # built-in function for likelihood of y
	lppdv[i]	<- 1/B*sum(liki)
	pwaicv[i]	<- (1/(B-1))*sum((log(liki) - mean(log(liki)))^2)
}

lppd		<- sum(log(lppdv))
pwaic	<- (1/(B-1))*sum(pwaicv)
WAIC		<- -2*lppd +2*pwaic


M	<- 2.56
curve(dbeta(x,6,3), from = 0, to = 1, col = 'blue', lwd = 8)
curve(M*dunif(x), add = TRUE, col = 'red', lwd = 8, lty = 2)

### Example II.R6 ###
M			<- 10
theta	<- vector(length = 1000)
arr		<- NULL
t			<- 1
count	<- 1

while(t < 1001){
	## step 1 ##
	tb	<- runif(1)
	U	<- runif(1)

	## step 2 ##
	r	<- dbeta(tb, 6, 3)/(M*dunif(tb))
	if(U < r){
		theta[t]	<- tb
		t			<- t + 1
	}
	count	<- count + 1
}

t/count
plot(density(theta), lwd = 8, col = 'blue')
curve(dbeta(x, 6, 3), add = TRUE, col = 'red', lwd = 8)
mean(theta)





