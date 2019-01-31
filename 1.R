require(ggplot2)
options(scipen = 999)

##########################################################################

set.seed(108)
n = 1000
u <- runif(n)
x = (1/27) * log(1 / (1-u) )
y <- rexp(n, rate = 27)
dat<-data.frame(x,y)

 
p <- ggplot(dat, aes(x)) + geom_histogram(col="#fbb4ae",fill="#decbe4", 
	bins = 111) + labs(x="Exponential from uniform") + 
	xlim( 0 , (summary(x)[6]  + mean(x)) )

q <- ggplot(dat, aes(y)) + geom_histogram(col="#ccebc5",fill="#b3cde3",
	bins = 111) + labs(x="Exponential from R",y="") + 
	xlim( 0 , (summary(y)[6]  + mean(y)) )

multiplot(p,q ,cols=2)


##########################################################################

set.seed(108)
n = 10000
u <- runif(n)
x = (1/27) * log(1 / (1-u) )
y <- rexp(n, rate = 27)
dat<-data.frame(x,y)

 
 
p <- ggplot(dat, aes(x)) + geom_histogram(col="#fbb4ae",fill="#decbe4", 
	bins = 111) + labs(x="Exponential from uniform") + 
	xlim( 0 , (summary(x)[6]  + mean(x)) )

q <- ggplot(dat, aes(y)) + geom_histogram(col="#ccebc5",fill="#b3cde3",
	bins = 111) + labs(x="Exponential from R",y="") + 
	xlim( 0 , (summary(y)[6]  + mean(y)) )

multiplot(p,q ,cols=2)




##########################################################################
##########################################################################


n = 1000
set.seed(108)
u <- runif(n)
x = 2 * tan( (pi * u ) - ( pi / 2 ) ) - 7
y <- rcauchy(n, location = -7, scale = 2)
dat<-data.frame(x,y)

p <- ggplot(dat, aes(x)) + geom_histogram(col="#3182bd",fill="#f7fcb9", 
	binwidth = .5) + labs(x="Cauchy from uniform") + 
	xlim( -10 , 10 )

q <- ggplot(dat, aes(y)) + geom_histogram(col="#636363",fill="#fdbb84",
	binwidth = .5) + labs(x="Cauchy from R",y="") + xlim(-10, 10)

multiplot(p,q ,cols=2)


##########################################################################

n = 10000
set.seed(108)
u <- runif(n)
x = 2 * tan( (pi * u ) - ( pi / 2 ) ) - 7
y <- rcauchy(n, location = -7, scale = 2)
dat<-data.frame(x,y)

p <- ggplot(dat, aes(x)) + geom_histogram(col="#3182bd",fill="#f7fcb9", 
	binwidth = .5) + labs(x="Cauchy from uniform") + 
	xlim( -20 , 20 )

q <- ggplot(dat, aes(y)) + geom_histogram(col="#636363",fill="#fdbb84",
	binwidth = .5) + labs(x="Cauchy from R",y="") + xlim(-20, 20)

multiplot(p,q ,cols=2)


##########################################################################
##########################################################################

require("evd")
rgev
n = 1000
set.seed(108)
u <- runif(n)
x = 3 - 6*log( -log( u ) )
y <- rgumbel(n, loc= 3 , scale= 6)
dat<-data.frame(x,y)

p <- ggplot(dat, aes(x)) + geom_histogram(col="#3182bd",fill="#f7fcb9", 
	binwidth = .5) + labs(x="Cauchy from uniform") + 
	xlim( -12 , 55 )

q <- ggplot(dat, aes(y)) + geom_histogram(col="#636363",fill="#fdbb84",
	binwidth = .5) + labs(x="Cauchy from R",y="")  + 
	xlim( -12 , 55 )


multiplot(p,q ,cols=2)

##########################################################################


n = 10000
set.seed(108)
u <- runif(n)
x = 3 - 6*log( -log( u ) )
y <- rgumbel(n, loc= 3 , scale= 6)
dat<-data.frame(x,y)

p <- ggplot(dat, aes(x)) + geom_histogram(col="#3182bd",fill="#f7fcb9", 
	binwidth = .5) + labs(x="Cauchy from uniform") + 
	xlim( -12 , 55 )

q <- ggplot(dat, aes(y)) + geom_histogram(col="#636363",fill="#fdbb84",
	binwidth = .5) + labs(x="Cauchy from R",y="")  + 
	xlim( -12 , 55 )


multiplot(p,q ,cols=2)
