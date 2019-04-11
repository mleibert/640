### Example II.R9 ###
laplace	<- read.table('laplace.txt', header = TRUE)
x			<- laplace$x

logPost	<- function(x, mu){ - sum(abs(x-mu)) }

set.seed(8562)
B			<- 2000
mu			<- vector("numeric", length = B)
arr		<- vector("numeric", length = B)
propVar	<- 0.5

### step 1 ###
mu[1]	<- median(x)

### step 2 ###
for(i in 2:B){
	muStar	<- rnorm(1, median(x), propVar)
	# muStar	<- rnorm(1, mu[i-1], propVar)
	r			<- exp(logPost(x, muStar))/exp(logPost(x, mu[i-1]))
	# r		<- exp(logPost(x, muStar) - logPost(x, mu[i-1]))
	u			<- runif(1)
	if(u < min(r,1)){
		mu[i]	<- muStar
		arr[i]	<- 1
	} else{
		mu[i]	<- mu[i-1]
		arr[i]	<- 0
	}
}

median(mu[-(1:(B/2))])
hist(mu[-(1:(B/2))], col = 'blue')
plot(mu[-(1:(B/2))], type = 'l', lwd = 4)

acf(mu[-(1:(B/2))])

mean(arr)


### Example II.R10 ###
post	<- function(x, lam, mu){
	sqrt(lam/(2*pi*x^3))*exp(-(lam*(x-mu)^2)/(2*(mu^2)*x))
}
curve(post(x, 1, 1), from = 0, to = 3, lwd = 8)
curve(dgamma(x, 1, 1), add = TRUE, lty = 2, col = 'aquamarine3', lwd = 8)

set.seed(882)
B			<- 10000
a			<- 2
b			<- 1
vec		<- vector("numeric", B)
x			<- a/b
vec[1]	<- x
ar			<- vector("numeric", B)

for (i in 2:B) {
	xstar 	<- rgamma(1, a, b)
	rho		<- (post(xstar, 1, 1)/post(x, 1, 1))/(dgamma(xstar, a, b)/dgamma(x, a, b))
	aprob 	<- min(1, rho)
	u 			<- runif(1)
	if (u < aprob){
		x 			<- xstar
		ar[i]	<- 1
	}
	vec[i] 	<- x
}

geweke.diag(vec[-(1:(B/2))])
plot(cumsum(vec[-(1:(B/2))])/(1:(B/2)), type = 'l', ylab = 'Running Mean', xlab = 'B', lwd = 8)
acf(vec[-(1:(B/2))])
mean(ar)






