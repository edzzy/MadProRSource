graphClustPval <-function(x){
	#-x est un vecteur de longeur 2 x[1] moyenne de la condition 1
	# x[2] moyenne de la condition 2
	# x[3] pvalue du test de student
	# si x[1] > 0 x[2] c'est up donc ont applique -log10(x[3])  >0
	# sinon c'est down est on applique log10(x[3]) <0
	if(!is.na(x[3])){
		if(x[1] > x[2]){
			r<- -log10(x[3])
		}else{
			r<-log10(x[3])

		}
	}else{
		r<-NA
	}
		return(r)
}
