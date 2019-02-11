## Example I.R4 ##
X			<- 27
n			<- 100
B			<- 10000

# Standard Uniform Prior #
set.seed(526)
rThetu	<- rbeta(B, X+1, n-X+1)

# Jeffreys' Prior #
set.seed(526)
rThetj	<- rbeta(B, X+1/2, n-X+1/2)

mean(rThetu)
mean(rThetj)

quantile(rThetu, probs = c(0.025, 0.975))
quantile(rThetj, probs = c(0.025, 0.975))

par(mfrow=c(1,2))
plot(density(rThetu), main = 'Standard Uniform', ylab = expression(paste('p(',theta,'|X)')), xlab = expression(theta), lwd = 8, col = 'blue')

plot(density(rThetj), main = "Jeffreys' Prior", ylab = expression(paste('p(',theta,'|X)')), xlab = expression(theta), lwd = 8, col = 'blue')


## non-informative inverse-gamma distributions ##
library(MCMCpack)
curve(dinvgamma(x, 1, 1), from = 0, to = 4, lwd = 2, col = 'blue')
curve(dinvgamma(x, 0.1, 0.1), from = 0, to = 4, lwd = 2, col = 'blue')
curve(dinvgamma(x, 0.01, 0.01), from = 0, to = 4, lwd = 2, col = 'blue')
curve(dinvgamma(x, 0.001, 0.001), from = 0, to = 4, lwd = 2, col = 'blue')

