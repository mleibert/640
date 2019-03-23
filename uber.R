setwd("G:\\math\\640")

uber <- read.csv("uber1.csv")
longlats <- read.csv("longlat.csv")

tail(uber)

length( unique( uber[,1] ) )

#install.packages("rjson")


library("rjson")
json_file <- "washington_DC_censustracts.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
names( json_data  )


uberdat <- list()
for( i in 1:length( json_data[[2]] ) ){
	uberdat[[i]] <- data.frame( json_data[[2]][[i]]$properties$MOVEMENT_ID , 
json_data[[2]][[i]]$properties$DISPLAY_NAME ) }

uberloc <- do.call("rbind", uberdat)
uberloc[,2] <- as.character(uberloc[,2])
colnames(uberloc) <- c("ID","address"  )
head(uberloc)

head(longlats)
uberloc <- cbind( uberloc, longlats[,2:3] )
head(uberloc )

  
#for( i in 1:nrow(uberloc)){locs[[i]]<-  as.data.frame(
#	geocode( uberloc[i,2]  ))  }


locss <- do.call("rbind", locs )

#write.csv( locss , "longlat.csv")

#

