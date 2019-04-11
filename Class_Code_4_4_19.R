### Example II.R11 ###
library(mcmcplots)
lap	<- read.table('laplace.txt', header = TRUE)
x	<- lap$x
mean(x)

B			<- 10000
tau		<- vector("numeric", B)
mu			<- vector("numeric", B)
ar			<- vector("numeric", B)
tau[1]	<- 1/sd(x)
m			<- mean(x)
mu[1]	<- m
n			<- length(x)
g			<- 0.5

fcmu	<- function(y, mu, tau){
	exp(-tau*sum(abs(y-mu)))
}

set.seed(7321)
for(t in 2:B){
	
	### sample tau ###
	tau[t]	<- rgamma(1, n, sum(abs(x-mu[t-1])))
	
	### sample mu ###
	mustar	<- rnorm(1, mu[t-1], g)
	# mustar	<- rnorm(1, mu[t-1], sqrt(1/(g*tau[t-1])))
	aprob 	<- min(1, (fcmu(x, mustar, tau[t-1])/fcmu(x, mu[t-1], tau[t-1]))/(dnorm(mustar, mu[t-1], sqrt(1/(g*tau[t-1])))/dnorm(mu[t-1], mustar, sqrt(1/(g*tau[t-1])))))
	U		<- runif(1)
	if(U < aprob){
		m			<- mustar
		ar[t]	<- 1
	}
	mu[t]		<- m
	
}

mean(ar)

acf(mu)

rmeanplot(mu[-(1:(B/2))])
geweke.diag(mu[-(1:(B/2))])
quantile(mu[-(1:(B/2))], probs = c(0.5, 0.025, 0.975))

rmeanplot(tau[-(1:(B/2))])
geweke.diag(tau[-(1:(B/2))])
quantile(tau, probs = c(0.5, 0.025, 0.975))


### Example II.R12 ###
# from Example II.R9 #
set.seed(90210)
post	<- function(x, lam, mu){
	sqrt(lam/(2*pi*x^3))*exp(-(lam*(x-mu)^2)/(2*(mu^2)*x))
}
B		<- 10000
a		<- 2
b		<- 1
vec1	<- vector("numeric", B)
x		<- 2
vec1[1]	<- x
ar		<- vector("numeric", B)
for (i in 2:B) {
	can 	<- rgamma(1, a, b)
	aprob 	<- min(1, (post(can, 1, 1)/post(x, 1, 1))/(dgamma(can, a, b)/dgamma(x, a, b)))
	u 		<- runif(1)
	if (u < aprob){
		x 		<- can
		ar[i]	<- 1
	}
	vec1[i] 	<- x
}

mean(ar)

vecb		<- vec[-(1:(B/2))]
vecb1	<- vec1[-(1:(B/2))]

chain1	<- mcmc(vecb[1:(length(vecb)/2)])
chain2	<- mcmc(vecb[(length(vecb)/2 + 1):length(vecb)])
chain3	<- mcmc(vecb1[1:(length(vecb1)/2)])
chain4	<- mcmc(vecb1[(length(vecb1)/2 + 1):length(vecb1)])

allChains <- mcmc.list(list(chain1, chain2, chain3, chain4)) 

gelman.diag(allChains)

# from 
lap	<- read.table('laplace.txt', header = TRUE)
x	<- lap$x
mean(x)

B			<- 10000
tau1		<- vector("numeric", B)
mu1			<- vector("numeric", B)
ar1			<- vector("numeric", B)
tau1[1]	<- 1/sd(x)
m			<- mean(x)
mu1[1]	<- min(x)
n			<- length(x)
g			<- 2

fcmu	<- function(y, mu, tau){
	exp(-tau*sum(abs(y-mu)))
}

set.seed(7321)
for(t in 2:B){
	
	### sample tau ###
	tau1[t]	<- rgamma(1, n, sum(abs(x-mu1[t-1])))
	
	### sample mu ###
	mustar	<- rnorm(1, mu[t-1], g)
	# mustar	<- rnorm(1, mu[t-1], sqrt(1/(g*tau[t-1])))
	aprob 	<- min(1, (fcmu(x, mustar, tau1[t-1])/fcmu(x, mu1[t-1], tau1[t-1]))/(dnorm(mustar, mu1[t-1], g)/dnorm(mu1[t-1], mustar, g)))
	U		<- runif(1)
	if(U < aprob){
		m			<- mustar
		ar[t]	<- 1
	}
	mu1[t]		<- m
	
}

mean(ar)

taub		<- tau[-(1:(B/2))]
taub1	<- tau1[-(1:(B/2))]

chain1	<- mcmc(taub[1:(length(taub)/2)])
chain2	<- mcmc(taub[(length(taub)/2 + 1):length(taub)])
chain3	<- mcmc(taub1[1:(length(taub1)/2)])
chain4	<- mcmc(taub1[(length(taub1)/2 + 1):length(taub1)])

allChainsT <- mcmc.list(list(chain1, chain2, chain3, chain4)) 

gelman.diag(allChainsT)

## Example II.R13 ##
# logistic regression via M-H #
library(mvtnorm)
oldFaithful	<- read.table('oldFaithful.txt', header = TRUE)
Y	<- oldFaithful$etime
n	<- length(Y)
X	<- cbind(rep(1,n), oldFaithful$waiting)

fit		<- glm(etime ~ waiting, data = oldFaithful, family = binomial(link = 'logit'))
bhat		<- coef(fit)
vbeta	<- vcov(fit)
B			<- 10000

beta		<- matrix(0, nrow = B, ncol = ncol(X))
ar			<- vector('numeric', length = B)

beta[1,]	<- bhat

tdens	<- function(b, X, Y){
	exp(sum(Y*(X%*%b) - log(1 + exp(X%*%b))))
}

tau	<- 0.001 # need to tune this

for(t in 2:B){
	
	# bstar	<- rmvnorm(1, beta[t-1,], tau*vbeta)
	bstar	<- rmvnorm(1, beta[t-1,], tau*diag(ncol(X)))
	r		<- tdens(t(bstar), X, Y)/tdens(beta[t-1,], X, Y)
	U		<- runif(1)
	if(U < min(1,r)){
		beta[t,]	<-bstar
		ar[t]		<- 1
	} else{
		beta[t,]	<- beta[t-1,]
		ar[t]		<- 0
	}
	
}

mean(ar[-c(1:(B/2))])

apply(beta[-c(1:(B/2)),], 2, quantile, probs = c(0.5, 0.025, 0.975))

## Example II.R14 ##
# Poisson regression via M-H #
seizure		<- read.table('seizures.txt', header = TRUE)
Y				<- seizure$y
n				<- length(Y)
trt			<- rep(0, n)
trt[seizure$trt == 'progabide']	<- 1
X				<- cbind(rep(1,n), trt, seizure$base, seizure$age)

fit			<- glm(y ~ trt + base + age, data = seizure, family = poisson(link = 'log'))
bhat			<- coef(fit)
vbeta		<- vcov(fit)
B				<- 10000

beta			<- matrix(0, nrow = B, ncol = ncol(X))
ar				<- vector('numeric', length = B)

beta[1,]	<- bhat

tdens		<- function(b, X, Y){
	exp(sum(Y*(X%*%b) - exp(X%*%b)))
}

tau			<- 1.5 # need to tune this

for(t in 2:B){
	
	bstar	<- rmvnorm(1, beta[t-1,], tau*vbeta)
	r			<- tdens(t(bstar), X, Y)/tdens(beta[t-1,], X, Y)
	U			<- runif(1)
	if(U < min(1,r)){
		beta[t,]	<-bstar
		ar[t]		<- 1
	} else{
		beta[t,]	<- beta[t-1,]
		ar[t]		<- 0
	}
	
}

mean(ar[-c(1:(B/2))])

coefs	<- t(apply(beta[-c(1:(B/2)),], 2, quantile, probs = c(0.5, 0.025, 0.975)))
rownames(coefs)	<- c('(Intercept)', 'Treatment', 'Baseline', 'Age')
coefs