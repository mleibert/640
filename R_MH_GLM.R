## logistic regression via M-H ##
oldFaithful	<- read.table('oldFaithful.txt', header = TRUE)
Y	<- oldFaithful$etime
n	<- length(Y)
X	<- cbind(rep(1,n), oldFaithful$waiting)

fit		<- glm(etime ~ waiting, data = oldFaithful, family = binomial(link = 'logit'))
bhat	<- coef(fit)
vbeta	<- vcov(fit)
B		<- 10000

beta		<- matrix(0, nrow = B, ncol = ncol(X))
ar			<- vector('numeric', length = B)

beta[1,]	<- bhat

tdens	<- function(b, X, Y){
	exp(sum(Y*(X%*%b) - log(1 + exp(X%*%b))))
}

tau	<- 3 # need to tune this

for(t in 2:B){
	
	bstar	<- rmvnorm(1, beta[t-1,], tau*vbeta)
	r		<- tdens(t(bstar), X, y)/tdens(beta[t-1,], X, Y)
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


## Poisson regression via M-H ##
seizure	<- read.table('seizures.txt', header = TRUE)
Y	<- seizure$y
n	<- length(Y)
trt	<- rep(0, n)
trt[seizure$trt == 'progabide']	<- 1
X	<- cbind(rep(1,n), trt, seizure$base, seizure$age)

fit		<- glm(y ~ trt + base + age, data = seizure, family = poisson(link = 'log'))
bhat	<- coef(fit)
vbeta	<- vcov(fit)
B		<- 10000

beta		<- matrix(0, nrow = B, ncol = ncol(X))
ar			<- vector('numeric', length = B)

beta[1,]	<- bhat

tdens	<- function(b, X, Y){
	exp(sum(Y*(X%*%b) - exp(X%*%b)))
}

tau	<- 1 # need to tune this

for(t in 2:B){
	
	bstar	<- rmvnorm(1, beta[t-1,], tau*vbeta)
	r		<- tdens(t(bstar), X, y)/tdens(beta[t-1,], X, Y)
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
