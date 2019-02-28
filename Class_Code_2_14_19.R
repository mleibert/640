## logistic regression via normal approximation ##
library(mvtnorm)
oldFaithful	<- read.table('oldFaithful.txt', header = TRUE)

fit		<- glm(etime ~ waiting, data = oldFaithful, family = binomial(link = 'logit'))

bhat		<- coef(fit)
vbeta	<- vcov(fit)
B			<- 10000

set.seed(123)
beta	<- rmvnorm(B, mean = bhat, sigma = vbeta)

# log odds #
t(apply(beta, 2, quantile, probs = c(0.5, 0.025, 0.975)))

# odds #
exp(t(apply(beta, 2, quantile, probs = c(0.5, 0.025, 0.975))))

## Poisson regression via normal approximation ##
seizure	<- read.table('seizures.txt', header = TRUE)

fit		<- glm(y ~ trt + base + age, data = seizure, family = poisson(link = 'log'))
bhat		<- coef(fit)
vbeta	<- vcov(fit)
B			<- 10000

set.seed(124)
beta		<- rmvnorm(B, mean = bhat, sigma = vbeta)

t(apply(beta, 2, quantile, probs = c(0.5, 0.025, 0.975)))

exp(t(apply(beta, 2, quantile, probs = c(0.5, 0.025, 0.975))))


