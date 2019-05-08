### Example II.R1 ###
set.seed(1875)

B 			<- 10000
rtheta	<- rbeta(B, 8, 14)
yobs		<- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

yrep		<- matrix(0, nrow = B, ncol = 20)
Tyrep	<- vector('numeric', length = B)

for(b in 1:B){
	yrep[b,] <- rbinom(length(yobs), 1, rtheta[b])
	Tyrep[b] <- length(rle(yrep[b,])$lengths) - 1
}

Ty			<- length(rle(yobs)$lengths) - 1
hist(Tyrep, xlab = expression(paste('T(', y^rep,',',theta,')')))
abline(v = Ty, col = 'blue', lwd = 2)

sum(Tyrep > Ty)/B

### Example II.R2 ###
library(MCMCpack)
library(moments)

setwd('/Users/mjm556/Desktop/MATH 640')
hers 	<- read.table('hers.txt', header = TRUE)
head(hers)

y			<- hers$x
ybar		<- mean(y)
sig		<- var(y)
n			<- length(y)
B			<- 10000

set.seed(3)

rmu		<- ybar + (sqrt(sig/n))*rt(B, df = n-1)
rsig		<- rinvgamma(B, (n-1)/2, ((n-1)/2)*sig)

yrep		<- matrix(0, nrow = B, ncol = length(y))
Tyrep	<- matrix(0, nrow = B, ncol = 4)

for(b in 1:B){
	yrep[b,]		<- rnorm(length(y), rmu[b], sqrt(rsig[b]))
	Tyrep[b,1]	<- mean(yrep[b,])
	Tyrep[b,2]	<- var(yrep[b,])
	Tyrep[b,3]	<- skewness(yrep[b,])
	Tyrep[b,4]	<- kurtosis(yrep[b,])
}

Ty1	<- mean(y)
Ty2	<- var(y)
Ty3	<- skewness(y)
Ty4	<- kurtosis(y)

sum(Tyrep[,1] > Ty1)/B
sum(Tyrep[,2] > Ty2)/B
sum(Tyrep[,3] > Ty3)/B
sum(Tyrep[,4] > Ty4)/B

hist(Tyrep[,4], xlim = c(min(Tyrep[,4]), 6))
abline(v=Ty4, col = 'blue')


### Example II.R3 ###
library(mcmcplots)

rmuMat					<- matrix(rmu, ncol = 1)
colnames(rmuMat)	<- 'mu'
rmuList					<- mcmc.list(list(mcmc(rmu)))

# trace plot #
traplot(rmuMat, greek = TRUE)

# running mean plot #
rmeanplot(rmuMat, greek = TRUE)

# ACF plot #
autplot1(rmuList)

# four-in-one plot #
mcmcplot1(rmuMat, greek = TRUE)

### Geweke Diagnostic ###
geweke.diag(rmu)
geweke.diag(rsig)

geweke.diag(rtheta)









