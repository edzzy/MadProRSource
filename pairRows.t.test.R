`pairRows.t.test` <-
function(comparaison_vect,mat,f,padj.method="none",pas=200,path=".",graph=TRUE,projet=""){
	f<-as.matrix(f)
	require(genefilter)
	mat<-as.matrix(mat)
	
	comparaison_name <-paste(comparaison_vect[1],comparaison_vect[2],sep="-Vs-")
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
	pval<-p.adjust(pval,method=padj.method)

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
		m1<-meanByFact(CC,ff,comparaison_vect[1])
		m2<-meanByFact(CC,ff,comparaison_vect[2])
		tabmean<-cbind(m1,m2,pval)
		pv<-apply(tabmean,1,graphClustPval)
	#	pv<-as.numeric(pval)
		graphMmobile(filename,pv,pas=pas,title=comparaison_name,seuil=0)
	}
  return(pval)
}

