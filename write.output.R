`writeMatrice` <-
function(matNorm,file){
	nomGenes<-row.names(matNorm)
	matFinale = cbind(nomGenes,matNorm)
	nomEchan<-colnames(matNorm)
	nomCol = c("GeneID",nomEchan)
	colnames(matFinale) = nomCol
	write.table(matFinale,file,sep="\t",col.names = TRUE,row.names = FALSE,quote=FALSE)
}

