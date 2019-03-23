setwd("G:\\math\\640")

uber <- read.csv("uber1.csv")

length( unique( uber[,1] ) )

install.packages("rjson")


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


uberloc[1,2]
locs<-list()
for( i in 1:nrow(uberloc) ){
locs[[i]] <- data.frame( 
strsplit(gsub(", ", ",",   uberloc[ i,2], fixed = T), ",",  )[[1]][1],
strsplit(gsub(", ", ",",   uberloc[ i,2], fixed = T), ",",  )[[1]][2],
strsplit(gsub(", ", ",",   uberloc[ i,2], fixed = T), ",",  )[[1]][3])
colnames(locs[[i]]) <- LETTERS[1:3]
}
locs<- do.call("rbind", locs)

locs<- apply( locs , 2, as.character) 
str(locs)

for( i in 1:nrow(locs) ){
	if( is.na( locs[i,3] ) == F ) { locs[i,2] <- locs[i,3] } }
	
locs <- locs[,-3]
locs <- as.data.frame(locs, stringsAsFactors = F)
 
locs$lon <- locs$lat <- rep(1,nrow(locs) )
 
locs <- list()
for( i in 1:nrow(uberloc)){locs[[i]]<-  as.data.frame(
	geocode( uberloc[i,2]  ))  }


locss <- do.call("rbind", locs )

write.csv( locss , "longlat.csv")

#ggmap::register_google(key = "AIzaSyCf2W9UY3uEOtPhJQei74bpJkbo8SpIPdw")

