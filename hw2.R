  eq = function(x){x^(-1)*(1-x)^(-.5)}
  plot(eq , xlim=c(-.1,2))

10e-11
eqq <- function(y){y^2}
integrate(eq,10e-23,1)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 


## 1A
X		<- 165
n		<- 716
m <-  n - X
curve(dbeta(x, X + 1, n - X + 1), from = 0, to = 1,
 xlab = expression(theta), ylab = expression(paste('p(',theta ,'|X)')),
 main = 'Posterior Density', lwd = 2, col = 'blue')

meanTheta	<- (X+1)/(n+2)
meanTheta

varTheta	<- ((X+1)*(n-X+1))/((n+3)*(n+2)^2)
varTheta

modeTheta	<- X/n
modeTheta


betaSamples	<- matrix(rbeta(10000, X + 1, n - X + 1), ncol = 1)
denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)

mean(betaSamples)
median(betaSamples)






## Example 1B
Y		<- 175
n		<- 919
 

betaSamples	<- matrix(rbeta(10000, X + Y , n - Y + m), ncol = 1)
colnames(betaSamples)	<- 'theta'

denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)

mean(betaSamples)
median(betaSamples)

m <-  n - Y + m




## 1C.a
Z		<- 142
n		<- 1289


betaSamples	<- matrix(rbeta(10000, Z + 1, n - Z + 1), ncol = 1)
colnames(betaSamples)	<- 'theta'

denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)

mean(betaSamples)
median(betaSamples)



## Example 1C.b

1289 - 142 + 1295
betaSamples	<- matrix(rbeta(10000, Z + Y + X , n - Z + m ), ncol = 1)
colnames(betaSamples)	<- 'theta'

denplot(betaSamples, main = "Sampled Posterior Density", lwd = 2)

credInt <- quantile(betaSamples, probs = c(0.025, 0.975))
densOut <- density(betaSamples)
idStart		<- max(which(densOut$x < credInt[1])) + 1
idEnd		<- min(which(densOut$x > credInt[2])) - 1
gx			<- densOut$x[idStart:idEnd]
gy			<- densOut$y[idStart:idEnd]
px			<- rep(0, length(gy))
polygon(c(gx, rev(gx)), c(px, rev(gy)), border = FALSE, col = rgb(0, 0, 1, alpha = 0.5))
abline(h = 0)

mean(betaSamples)
median(betaSamples)

