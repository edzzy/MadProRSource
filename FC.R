FC <-function(x){
#Calcule le fold change entre deux valeurs

	if(length(x) != 2 | x[1] <=0 | x[2] <= 0 | is.character(x)){
		stop("Longeur de x doit etre de 2")		
	}else{

		if(x[1] >= x[2]){
			r<- x[1]/x[2]
		}else{
			r<-  x[2]/x[1]
			r<- -r
		}
		return(r)
	}
}
