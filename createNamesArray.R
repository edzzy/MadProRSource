`createNamesArray` <-
function(frameSample,type="AG",dye=2,ratio=NULL){
	#Description : fonction qui cree le nom des echantillons
	#OUTPUT :Retourne un dataframe : une colonne nR pour les identifiants Cy5 et une nG pour les identifiants Cy3.
		#Les identifiants sont formate comme ceci : ID_CodeBarPuce_NumerosArray.
	#INPUT : data.frame avec au moins les colonnes Cy5ID ; Cy3ID ; cbPuce ; Array
	if(missing(frameSample))
		stop("argument frameSample est manquant")
		arraySample<-""
	if(type == "AG" && dye == 2){
		if(is.null(frameSample$Cy5ID))
			stop("La colonne Cy5ID n'existe pas !! verifier le fichier d'annotations des echantillons")
		if(is.null(frameSample$Cy3ID))
			stop("La colonne Cy3ID n'existe pas !! verifier le fichier d'annotations des echantillons")
		if(is.null(frameSample$cbPuce))
			stop("La colonne cbPuce n'existe pas !! verifier le fichier d'annotations des echantillons")
		if(is.null(frameSample$Array))
			stop("La colonne Array n'existe pas !! verifier le fichier d'annotations des echantillons")
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
		#Identifiant des echantillons en Cy3 et Cy5
		nR<-frameSample$Cy5ID
		nG<-frameSample$Cy3ID
		#Reccupere les derniers chiffres du code bar de la Puce
		cb<-substr(frameSample$cbPuce,nchar(frameSample$cbPuce)-4,nchar(frameSample$cbPuce))
		#Reccupere le cadran de la puce
		ar<-frameSample$Array
		#Colle les different element pour faire un nom unique de type Id_codeBar_NumeroCadran
		nR<-paste(nR,cb,ar,sep="_")
		nG<-paste(nG,cb,ar,sep="_")
		arraySample<-data.frame(nR,nG)
	}else if(type == "AG" && dye == 1){
		if(is.null(frameSample$ID))
			stop("La colonne ID n'existe pas !! verifier le fichier d'annotations des echantillons")
		if(is.null(frameSample$cbPuce))
			stop("La colonne cbPuce n'existe pas !! verifier le fichier d'annotations des echantillons")
		if(is.null(frameSample$Array))
			stop("La colonne Array n'existe pas !! verifier le fichier d'annotations des echantillons")
		#Identifiant des echantillons en Cy3 et Cy5
		ID<-frameSample$ID
		#Reccupere les derniers chiffres du code bar de la Puce
		cb<-substr(frameSample$cbPuce,nchar(frameSample$cbPuce)-4,nchar(frameSample$cbPuce))
		#Reccupere le cadran de la puce
		ar<-frameSample$Array
		#Colle les different element pour faire un nom unique de type Id_codeBar_NumeroCadran
		ID<-paste(ID,cb,ar,sep="_")
		arraySample<-ID

	}else if(type=="NG"){

	
	}else if(type=="GPR"){


	}else if(type=="AFFY"){

	}
	return(arraySample)
}

