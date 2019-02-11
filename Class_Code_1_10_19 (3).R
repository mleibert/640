## Example I.R1 ##
set.seed(1212)
x		<- rnorm(1000)

plot(density(x), lwd = 8, lty = 2, ylim = c(0,0.4))
curve(dnorm(x), add = TRUE, col = 'blue', lwd = 8)



## Example I.R2 ##
set.seed(327)
B		<- 1000
y		<- rpois(B,3)

plot(0:15, dpois(0:15, 3), type = 'h', ylim=c(0.,0.25), lwd = 8)
points(0:15, dpois(0:15, 3), pch = 16, cex = 4)
lines(0:(length(table(y))-1), table(y)/B, col = 'blue', type = 'h', lwd = 8)
points(0:(length(table(y))-1), table(y)/B, pch = 16, col ='blue', cex = 4)

