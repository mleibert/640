rm(list=ls());gc(); dev.off()
setwd("G:\\math\\640")


post <- function(x,B){( B/(2*gamma(1/B)))^length(x) * exp(-sum(abs(x)^B) ) }
reign <- read.table("coup1280.txt", stringsAsFactors = F , header =T)
tail(reign)
z <- scale(reign[,2])

postz <- function( B){   (   ( B/( gamma(1/B)))^length(z)  * exp(-sum( (abs(z))^B) ) ) }
post <- function(a,...){ return(sapply(a,postz,...)) }


 curve( post , from = .9 , to = 2 )
  
 M <- optimize( post  , lower = 0, upper = 4, maximum=T ) 
 abline(v =M)

 fy2 <- function( x , y ){ post(  x) /  dnorm(x,mean = 2, sd = 1)  }
 
 fy2 <- function( x , y ){ fy(hers0$y, x) / dcauchy(x, median(hers0$y) ) }
 optimize( fy2 , lower =0, upper = 2, maximum=T )
 
#  
# curve( M * ( dcauchy(x, median(z) )) , add = T )
# require("pgnorm")
# ppgnorm(  z , 2,  1 , 1 )
# pnorm(z)
#  
# curve(dpgnorm(z))
# 
# postz <- function( B){   (   ( B/( gamma(1/B)))^length(z)  * exp(-sum( (abs(z))^B) ) ) }
# curve(  mse.plottable(x)  , from = 1 , to = 2  )
# curve(dnorm(x,mean = 2, sd = 1)   , from = 1 , to = 5 , add = T)
# 
# fy2 <- function( x , y ){ postz(  x) /  dnorm(x,mean = 2, sd = 1)  }
# M <- optimize( mse.plottable , lower = -1, upper = 4, maximum=T )
# abline(v = M)
# 
# 
# postz(2) / 2e+73
# 
# optim(par = 1, fn = postz ,lower = 0, upper = 4 ) 
# optimize(postz, lower = 0 , upper= 4, maximum = T)
# 
# bb <- 1:4
# exp(-sum( (abs(zz))^bb) ) 
# mse.plottable <- function(a,...){ return(sapply(a,postz,...)) }
# 
