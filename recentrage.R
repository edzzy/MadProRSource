recentrage<-function(matrice, facteur,l=FALSE){
	if(missing(matrice)){
		stop("l'argument matrice est manquant")
	}
	if(missing(facteur)){
		stop("l'argument facteur est manquant")
	}
	facteur<-as.factor(as.character(unlist(facteur)))
	mrecentrer<-matrix()
	matrice<-as.matrix(matrice)
	param<-levels(facteur)
	nbParam<-nlevels(facteur)
	
	for (i in 1:nbParam){
		tmpM<-matrice[,which(facteur==param[i])]
		if(l){
			pMed<-apply(tmpM, 1, median)
			tmpMrc<- log10(tmpM/pMed) 
		}else{
			tmpMrc<- tmpM - apply(tmpM, 1, median)
		}
		if( i == 1 ){
			mrecentrer<-tmpMrc
		}else{
			mrecentrer<-cbind(mrecentrer,tmpMrc)
		}
	}
	return(mrecentrer)
}
