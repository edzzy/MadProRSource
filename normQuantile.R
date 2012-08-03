`normQuantile` <-
function(mat,pngDir){

	nomEchan = colnames(mat)
	matN<-normalizeBetweenArrays(as.matrix(mat),method="quantile")
	profMed<-apply(mat,1,median)
	profMedN<-apply(matN,1,median)	

	increm=c(1:dim(mat)[2])
########## Plots ###########################
  	diagonal=c(min(profMed,na.rm=T), max(profMed,na.rm=T))
  	
  	# Graphs avant normalisation pour tous les echantillons
  	graph=sapply(increm, traceGraph, mat, profMed, diagonal, NULL ,nomEchan,"1-Avant", pngDir)
		
  	# Graphs apres normalisation pour tous les echantillons
  	graph=sapply(increm, traceGraph, matN, profMedN, diagonal, NULL,nomEchan,"2-Apres", pngDir)
	
  	#Graph valeurs brutes/valeurs normalisees
  	graph=sapply(increm, traceAvantApres, mat, matN, nomEchan, pngDir)
return(matN)
}

