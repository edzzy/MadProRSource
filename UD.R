`UD` <-
function(x){
	r<-"down"
	if(x[1] > x[2]){
		r<-"up"
	}else{
		r<-"down"
	}
	return(r)
}

