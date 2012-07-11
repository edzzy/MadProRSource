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
#	matNormlog = sapply(increm, calculeNorm, log(matOrdonne), lowessCurve, log(profilOrdonne))
#	matNorm = sapply(increm, calculeNorm, log(matOrdonne), lowessCurve, log(profilOrdonne))
#	matNorm=exp(matNormlog) 	
	matNorm = sapply(increm, calculeNorm, matOrdonne, exp(lowessCurve), profilOrdonne)
	
	profilMedNorm = apply(matNorm,1,median,na.rm=TRUE)
	
	
  if(graph==1){	
  	########## Plots ###########################
  	diagonal=c(min(profilOrdonne,na.rm=T), max(profilOrdonne,na.rm=T))
  	
  	# Graphs avant normalisation pour tous les echantillons
  #	dev.set(dev.next())
  	graph=sapply(increm, traceGraph, matOrdonne, profilOrdonne, diagonal, lowessCurve,nomEchan,"1-Avant", pngDir)
		
  	# Graphs apres normalisation pour tous les echantillons
  #	dev.set(dev.next())
  	graph=sapply(increm, traceGraph, matNorm, profilMedNorm, diagonal, NULL,nomEchan,"2-Apres", pngDir)
	
  	#Graph valeurs brutes/valeurs normalisees
  #	dev.set(dev.next())
  	graph=sapply(increm, traceAvantApres, matOrdonne, matNorm, nomEchan, pngDir)
	} ## If graph
	##############################
	
	### Exportation des resultats	
	rownames(matNorm)=nomGenes
	colnames(matNorm) = nomEchan
	matNorm<-round(matNorm,2)
	
	nomFile = paste(pngDir,projet,"-",nom_fichier, "-normalisation.txt", sep="")
	write.table(matNorm,nomFile,sep="\t",row.names=TRUE, col.names=NA)
	print("Fonction LOEWESS!!")
  return (matNorm)
}

