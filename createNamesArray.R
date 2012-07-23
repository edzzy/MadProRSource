`createNamesArray` <-
function(pData){
	#Description : fonction qui cree le nom des echantillons
	#OUTPUT :Retourne un dataframe : une colonne nR pour les identifiants Cy5 et une nG pour les identifiants Cy3.
		#Les identifiants sont formate comme ceci : ID_CodeBarPuce_NumerosArray.
	#INPUT : data.frame avec au moins les colonnes Cy5ID ; Cy3ID ; cbPuce ; Array
	if(missing(pData))
		stop("argument frameSample est manquant")
      
		#Identifiant des echantillons en Cy3 et Cy5
		nR<-rownames(pData)[which(tolower(pData$Dye)== "cy5")]
		nG<-rownames(pData)[which(tolower(pData$Dye)== "cy3")]
		arraySample<-data.frame(nR,nG);
		return(arraySample)
}

