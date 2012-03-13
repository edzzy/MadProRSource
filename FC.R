FC <-function(x){
	if(x[1] > x[2]){
		r<- x[1]/x[2]
 	}else{
		r<-  x[2]/x[1]
		r<- -r
	}
	return(r)
}
