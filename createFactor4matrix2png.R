`createFactor4matrix2png` <-
function(frameSample,namesArray,dye=2,ratio=NULL){
	##TODO changer le nom de la fonction en setArrayFactor
	##TODO Solution pour du mono couleur
	#DESCRIPTION Creer le tableau des parametres pour les echantillons
	#INPUT
		#frameSample data.frame ayant 5 colonnes fixes (nomFichiers ; cbPuce ; Array ; Cy5ID ; Cy3ID)
			#puis des colonnes pour chaque parametre (2 par parametres une pour Cy3 et une pour Cy5)
		#namesArray data.frame avec 2 colonnes une nR et une nG qui correspond aux noms des echantillons pour Cy5 et Cy3
	#OUTPUT un dataframe avec autant de colonne que d'echantillons et autant de ligne que de parametres.
  
  if(missing(frameSample))
		stop("Argument frameSample manquant")
	if(missing(namesArray))
		stop("Argument namesArray manquant")
	if(dye == 2){
		if(is.null(frameSample$Cy5ID))
			stop("colonne Cy5ID dans le tableau namesArray")
		if(is.null(frameSample$Cy3ID))
			stop("colonne Cy3ID dans le tableau namesArray")
		if(ncol(frameSample) <= 5)
			stop("Probleme avec le nombre de colonne du tableau frameSample : nombre de colonnes inferieur a 5")
	
    if(ratio=="TRUE"){
      if(is.null(frameSample$Cy5Ratio))
    	  stop("La colonne Cy5Ratio n'existe pas !! verifier le fichier d'annotations des echantillons")
      if(is.null(frameSample$Cy3Ratio))
  		  stop("La colonne Cy3Ratio n'existe pas !! verifier le fichier d'annotations des echantillons")
      if(is.null(frameSample$Cy5Paire))
  		  stop("La colonne Cy5Paire n'existe pas !! verifier le fichier d'annotations des echantillons")
      if(is.null(frameSample$Cy3Paire))
  		  stop("La colonne Cy3Paire n'existe pas !! verifier le fichier d'annotations des echantillons")
    }
    
		  #Calcul le nombre de parametre dans le tableau (2 colonnes par parametre)
		  nbC<-( ncol(frameSample) - 5)
		  nbP<-nbC/2
		
		  #tableau avec les parametre des echantillons
		  frameFac<-data.frame(frameSample[,6:ncol(frameSample)])
		  ncf<-ncol(frameFac)
		  #initialisation avec le premier parametre
		  fac<-c(as.character(unlist(frameFac[,1])),as.character(unlist(frameFac[,2])))
		  dataFac<-data.frame(fac)
		  #nom des echantillons
		  rNames<-c(as.character(unlist(namesArray$nR)),as.character(unlist(namesArray$nG)))
		  rownames(dataFac)<-rNames
		  if(nbP != 1){  
			  #numeros de colonne de parametre pour Cy5 (sans le premier parametre)
			  seqC<-seq(from=3,to=ncf-1,by=2)
			  for(i in seqC ){
				  fac<-c(as.character(unlist(frameFac[,i])),as.character(unlist(frameFac[,i+1])))
				  dataFac<-data.frame(dataFac,fac)
          
			  }
        if(!is.null(ratio)){
          colnames(dataFac)[1] = "Ratio"
          colnames(dataFac)[2] = "Paire"
        }
		  }
    
	}else{
		if(is.null(frameSample$ID))
			stop("Pas de colonne ID dans le tableau namesArray (fichier annotation Des echantillons")
		if(ncol(frameSample) <= 4)
			stop("Probleme avec le nombre de colonne du tableau frameSample : nombre de colonnes inferieur a 5")
			#Calcul le nombre de parametre dans le tableau (2 colonnes par parametre)
		nbP<-( ncol(frameSample) - 4)
		
		#tableau avec les parametre des echantillons
		frameFac<-data.frame(frameSample[,5:ncol(frameSample)])
		ncf<-ncol(frameFac)
		#initialisation avec le premier parametre
		fac<-c(as.character(unlist(frameFac[,1])))
		dataFac<-data.frame(fac)
		#nom des echantillons
		rNames<-c(as.character(unlist(namesArray)))
		rownames(dataFac)<-rNames
		if(nbP != 1){  
			#numeros de colonne de parametre pour Cy5 (sans le premier parametre)
			seqC<-seq(from=2,to=ncf,by=1)
			for(i in seqC ){
				fac<-c(as.character(unlist(frameFac[,i])))
				dataFac<-data.frame(dataFac,fac)
			}
		}	
	}
	
	return(t(dataFac))
}

