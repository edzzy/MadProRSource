CV<-function(matrice){
    if(missing(matrice)){              #test si la matrice est presente
        stop("L'argument matrice est manquant")
    }
    matrice<-as.matrix(matrice)
    ecartType<-apply(matrice,1,sd)                                                                                           
    moyenne<-apply(matrice,1,mean)
    CV<-ecartType/moyenne
    return(CV)
}

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
`getUpDown` <-
function(vPval,preFileName){
	vPval<-tolower(vPval)
	up<-vPval[which(vPval=="up")]
	down<-vPval[which(vPval=="down")]
	upName<-paste(preFileName,"-Up.txt",sep="")
	downName<-paste(preFileName,"-down.txt",sep="")
	write.table(up,upName,col.names=NA,sep="\t",quote=FALSE)
	write.table(down,downName,col.names=NA,sep="\t",quote=FALSE)
	

}

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
`pairRows.t.test` <-
function(comparaison_vect,mat,f,padj.method="none",pas=200,path=".",graph=TRUE,projet=""){
	f<-as.matrix(f)
	require(genefilter)
	mat<-as.matrix(mat)
	
	comparaison_name <-paste(comparaison_vect[1],comparaison_vect[2],sep="-VS-")
	finale<-data.frame(row.names(mat))
	
	#CC <- mat[,which(f==comparaison_vect[1] | f == comparaison_vect[2])]
	 #if(ncol(f) == 1){
    	ff <- f[which(f==comparaison_vect[1] | f == comparaison_vect[2])]
    
  	#}else{
	 #ff <- f[,which(f==comparaison_vect[1] | f == comparaison_vect[2],arr.ind=TRUE)[,2]]
  	 #ftmp <- f[,which(f==comparaison_vect[1],arr.ind=TRUE)[,2]]
  	 #ff <- ftmp[,which(ftmp==comparaison_vect[2],arr.ind=TRUE)[,2]]
  	#}
	
	CC <- mat[,which(f==comparaison_vect[1] | f == comparaison_vect[2],arr.ind=TRUE)[,2]]

	
  
	ff<-as.factor(ff)
	tt<-rowFtests(CC,ff,var.equal=FALSE)
	pval<-tt$p.value
	names(pval)<-rownames(CC)
	pvaladjust<-p.adjust(pval,method=padj.method)
	pvalData<-cbind(pval,pvaladjust)
	colnames(pvalData)<-c("PvalRaw","PvaAdjust-BH")
#	if(!is.null(pval[which(is.na(pval))])){
#		probeNa<- names(pval[which(is.na(pval))])
#		pvalNa<-pval[which(is.na(pval))]
#		names(pvalNa)<-probeNa
#		write.table(pvalNa,file=paste(path,"/probeNA",sep=""))
#		warning("Le test stat de ces sondes : ", probeNa, " ont generes des pvalue : NA")
#	}

	filename=paste(path,"/",projet,comparaison_name,".png",sep="")
	nameFile=paste(path,"/",projet,"-allPval-",comparaison_name,".txt",sep="")

	if(graph==TRUE){
		m1<-meanByFact(mat,f,comparaison_vect[1])
		m2<-meanByFact(mat,f,comparaison_vect[2])
		tabmean<-cbind(m1,m2,pval)
		pv<-apply(tabmean,1,graphClustPval)
	#	pv<-as.numeric(pval)
		graphMmobile(filename,pv,pas=pas,title=comparaison_name,seuil=0)
	}
  return(pvalData)
}

`testLogRatio` <-
function(mat,pas=200,projet=NULL,path="./"){
	result_ttest<-apply(-log10(mat),1,t.test,mu=0)
	p<-result_ttest[[1]]$p.value
	for (i in 2:length(result_ttest)){
		p<-c(p,result_ttest[[i]]$p.value)
	}
  
	filename<-paste(path,"/",projet,"-Ratio.png",sep="")
	graphMmobile(filename,-log10(p),pas=pas)

	names(p)<-rownames(mat)
	return(p)
}

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

