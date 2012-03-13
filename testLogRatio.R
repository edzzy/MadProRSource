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

