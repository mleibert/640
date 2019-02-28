## Example I.R5 ##
# install.packages('MCMCpack')
library(MCMCpack)

setwd('/Users/mjm556/Desktop/MATH 640')
hers <- read.table('hers.txt', header = TRUE)
head(hers)

y		<- hers$x
ybar	<- mean(y)
sig	<- var(y)
n		<- length(y)
B		<- 10000

set.seed(3)

rmu		<- ybar + (sqrt(sig/n))*rt(B, df = n-1)
rsig		<- rinvgamma(B, (n-1)/2, ((n-1)/2)*sig)

mean(rmu)
mean(rsig)

quantile(rmu, probs = c(0.025, 0.975))
quantile(rsig, probs = c(0.025, 0.975))

par(mfrow = c(1,2))
plot(density(rmu), xlab = expression(mu), col = 'blue', lwd = 8)
plot(density(rsig), xlab = expression(sigma^2), col = 'blue', lwd = 8)


## Example I.R6 ##
set.seed(3)
B				<- 10000
rsig.R6		<- rinvgamma(B, (n-1)/2, ((n-1)/2)*sig)
rmu.R6		<- rnorm(B, ybar, sqrt(rsig.R6/n))

mean(rmu.R6)
mean(rsig.R6)

# compare results #
quantile(rmu, probs = c(0.5, 0.025, 0.975))
quantile(rmu.R6, probs = c(0.5, 0.025, 0.975))

quantile(rsig, probs = c(0.5, 0.025, 0.975))
quantile(rsig.R6, probs = c(0.5, 0.025, 0.975))

par(mfrow = c(1,2))
plot(density(rmu.R6), xlab = expression(mu), col = 'blue', lwd = 10)
lines(density(rmu), col = 'green', lwd = 10, lty = 2)
plot(density(rsig.R6), xlab = expression(sigma^2), col = 'blue', lwd = 10)
lines(density(rsig), col = 'green', lwd = 10, lty = 2)


## Example I.R7 ##
set.seed(2584)
B				<- 10000
postAlpha	<- c(338 + 1, 296 + 1, 282 + 1, 493 + 1)
rtheta 		<- rdirichlet(B, postAlpha)

apply(rtheta, 2, mean)
out	<- t(apply(rtheta, 2, quantile, probs = c(0.5, 0.025, 0.975)))
rownames(out)	<- c('LePen', 'Fillon', 'Macron', 'Other')
out

plot(density(rtheta[,1]), col = 'blue', lwd = 2, main = '', ylab = expression(paste('p(',theta[k],'|',y[k],')')), xlab = expression(theta[k]), xlim = c(0.15, 0.45), ylim = c(0, 40))
lines(density(rtheta[,2]), col = 'green', lwd = 2, lty = 2)
lines(density(rtheta[,3]), col = 'red', lwd = 2, lty = 3)
lines(density(rtheta[,4]), col = 'purple', lwd = 2, lty = 4)

mean(rtheta[,3] >= 0.2401)




