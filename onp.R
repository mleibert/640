OnlineNewsPopularity


setwd("G:\\math\\640")

onp <- read.csv("OnlineNewsPopularity.csv")
nrow(onp)
onp <- onp[ , -which(colnames(onp) %in% c("LDA_00" , "LDA_01", "LDA_02",  
	"LDA_03", "LDA_04") )   ]
nrow(onp)

onp <- onp[ , -c(which( colnames(onp) == "weekday_is_monday" ):
	which( colnames(onp) == "weekday_is_sunday" ) ) ]
nrow(onp)


 
tdf <- onp[ , which(colnames(onp) == "data_channel_is_lifestyle"):
which(colnames(onp) == "data_channel_is_world") ]
namez <- substring( colnames(tdf)  , nchar("data_channel_is_" )+1  )
tdf$topic <- NA

for( i in 1:nrow(tdf) ){ 
	if( sum( tdf[i,-ncol(tdf)] ) == 0) {tdf$topic[i] <- "other"; next}
	tdf$topic[i] <- namez[ which( tdf[i,] == 1 ) ] }


onp$topic <- tdf$topic
rm(tdf)
onp<- onp[ , -c(which(colnames(onp) == "data_channel_is_lifestyle"):
which(colnames(onp) == "data_channel_is_world")) ]
head(onp)
onp$topic <- as.factor(onp$topic )

#write.csv( onp , "onp.csv" )

head(onp)

nchar("http://mashable.com/")
nchar("2013/01/09")




datez <- substring(onp[,1] ,nchar("http://mashable.com/")+1 , 
	nchar("http://mashable.com/")+10 )



onp$Weekdays <- weekdays( as.Date( datez )  ) 
tail(onp)

table(onp$Weekdays)

onp$kw_min_min[ ]

onp[79:80,]
onp[ which( onp$shares == max( onp$shares ) )  ,]

onp[16615:16654,]

 
cor( onp[ , which( colnames(onp) == "kw_min_min"):
which( colnames(onp) ==  "kw_avg_avg" )  ] )

onp$ID <- 1:nrow(onp)
onpS <- onp[ sample( 1:nrow(onp) , 9644 ) , ]
onpT <- onp[  -onpS$ID, ] 

nrow(onpT );nrow(onpS )
head(onpT)
fit <- glm(  shares ~ . -ID , family = gaussian, data= onpT[,-c(1:2) ])
summary( fit )

onpS$SHARES <- ( predict( fit  , newdata = onpS[,-c(1:2) ] ) )

plot(  onpS$shares ,onpS$SHARES  ) 

hist( onpS$shares - onpS$SHARES   )
require(glmnet)

data("PimaIndiansDiabetes")
X = model.matrix(shares ~ . -ID , data=onpS[,-c(1:2) ])

Y = onpS$shares 
 

library(MASS)
fit.negbin = glm.nb(shares ~ . -ID , data=onpS[,-c(1:2) ])

1-pchisq(9597, 9643)

onpS$SHARES <- exp( predict( fit.negbin , newdata = onpS[,-c(1:2) ] ) )
 
head( onpS[,-c((ncol(onpS)-2):(ncol(onpS)-1))], 100 )


# cv.glmnet is the main function to do cross-validation.
# Here we use it with the misclassification error as criterion.
# Other options include "deviance" and "auc"

cvfit = cv.glmnet(x=X[,-1], y=Y, family="poisson", type.measure="deviance")
plot(cvfit)

lambda_1se = cvfit$lambda.1se
coef(cvfit, s=lambda_1se)



