meanByFact<-function(mat,fac,l){
	mat<-as.matrix(mat)
	#fac<-as.factor(unlist(fac))
	fac<-as.matrix(fac)
	matFac<-mat[,which(fac == as.character(l),arr.ind=TRUE)[,2]]
	meanFac<-apply(matFac,1,mean)
	return(meanFac)

}
