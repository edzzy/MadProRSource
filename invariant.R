invariant<-function(matrice, inv){

	if(missing(matrice)){
		stop("L'argument matrice est manquant")	
	}
	if(missing(inv)){
		stop("L'argument inv est manquant")	
	}
	seuil_inv_quant<-quantile(as.matrix(matrice), seq(from=0, to=1, by=1/inv))
	cv<-CV(matrice)
	profMedian<-apply(matrice, 1, median)
	i_quant<-list()	
	for( i in 1:length(seuil_inv_quant)-1){
		i_quant<-cv[which(profMedian > seuil_inv_quant[i] & profMedian < seuil_inv_quant[i+1])]
		l<-length(i_quant)
		print(l)
		name<-paste("quantile",i,"-",inv,".txt",sep="")
		sort(i_quant)
		write.table(i_quant,name,row.names=TRUE, col.names=FALSE, sep="\t")
		
	}
}
