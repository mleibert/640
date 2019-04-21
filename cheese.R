cheese<-read.table("cheese.txt",header=T)
cheese.ungrp = as.data.frame(lapply(cheese, 
	function(x,p) rep(x,p), cheese$count))


