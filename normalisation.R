`calculeNorm` <-
function(increm, echantillon, lowessCurve, ref)
{
	result = echantillon[,increm]/lowessCurve[,increm]*ref
	return(result)
}

`LOWESS` <-
function(nom_fichier,data,pngDir,profil.median="NA",graph=1,projet=stop("nom de projet manquant"))
{
	
	nomGenes=rownames(data)
	nomEchan = colnames(data)
	
	# Calcul du profil median
	if(profil.median=="NA"){
	   profil.median = apply(data,1,median,na.rm=TRUE)
	}
	
	######### Tri par ordre croissant des profils medians  #######
	
	#On ordonne la matrice par ordre croissant des profils median
	ordre.profil = order(profil.median)
	
	#On range la matrice de depart
	matOrdonne = data[ordre.profil,]
	
	#On range le vecteur de profil median
	profilOrdonne = profil.median[ordre.profil]
	
	#On range le vecteur des noms de genes
	nomGenes = as.character(nomGenes)
	nomGenes = nomGenes[ordre.profil]	
	########################################################
	
	############# Lowess #######################	
	#Matrice lowess
	# On passe les donnees en log
	#pngDir<-"../,"02-lowess/"
	lowessCurve = NULL
	lowessCurve = cbind(lowessCurve,apply(log(matOrdonne),2,my.lowess,log(profilOrdonne),f=0.01))
	
	# Calcul de la matrice normalisee
	increm=c(1:dim(data)[2])
	matNorm = sapply(increm, calculeNorm, matOrdonne, exp(lowessCurve), profilOrdonne)
	
	profilMedNorm = apply(matNorm,1,median,na.rm=TRUE)
	
	
  if(graph==1){	
  	########## Plots ###########################
  	diagonal=c(min(profilOrdonne,na.rm=T), max(profilOrdonne,na.rm=T))
  	
  	# Graphs avant normalisation pour tous les echantillons
  	graph=sapply(increm, traceGraph, matOrdonne, profilOrdonne, diagonal, lowessCurve,nomEchan,"1-Avant", pngDir)
		
  	# Graphs apres normalisation pour tous les echantillons
  	graph=sapply(increm, traceGraph, matNorm, profilMedNorm, diagonal, NULL,nomEchan,"2-Apres", pngDir)
	
  	#Graph valeurs brutes/valeurs normalisees
  	graph=sapply(increm, traceAvantApres, matOrdonne, matNorm, nomEchan, pngDir)
	} ## If graph
	##############################
	
	### Exportation des resultats	
	rownames(matNorm)=nomGenes
	colnames(matNorm) = nomEchan
	matNorm<-round(matNorm,2)
	
  return (matNorm)
}

`my.lowess` <-
function(matriceW,vecRef,f)
{
	index = which(is.na(matriceW))
	indexVal = which(!is.na(matriceW))

	if (length(index)>1) {
		print("OT des NAs")
		print(length(matriceW))
		vecRef = vecRef[-index]
		matriceW2 = matriceW[-index]

		result = lowess(vecRef,matriceW2,f=f)
		temp = result$y

		sortie = rep(NA,length(matriceW))
		sortie[indexVal]=temp
		print(length(sortie))
	}

	else{
		result = lowess(vecRef,matriceW,f=f)
		sortie = result$y
	}
	sortie
}

`normQuantile` <-
function(mat,pngDir,img=TRUE){

	nomEchan = colnames(mat)
	matN<-normalizeBetweenArrays(as.matrix(mat),method="quantile")
	profMed<-apply(mat,1,median)
	profMedN<-apply(matN,1,median)	

	increm=c(1:dim(mat)[2])
########## Plots ###########################
  	diagonal=c(min(profMed,na.rm=T), max(profMed,na.rm=T))
  if(img == TRUE){	
  	# Graphs avant normalisation pour tous les echantillons
  	graph=sapply(increm, traceGraph, mat, profMed, diagonal, NULL ,nomEchan,"1-Avant", pngDir)
		
  	# Graphs apres normalisation pour tous les echantillons
  	graph=sapply(increm, traceGraph, matN, profMedN, diagonal, NULL,nomEchan,"2-Apres", pngDir)
	
  	#Graph valeurs brutes/valeurs normalisees
  	graph=sapply(increm, traceAvantApres, mat, matN, nomEchan, pngDir)
	 }
return(matN)
}

