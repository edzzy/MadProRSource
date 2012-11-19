`read_Agilent` <-
function(pData,namesGenes,type="AG",pathDir=""){
#Description : La fonction renvoie une matrice d'expression en lisant les fichiers
			# issus de logiciel d'extraction d'image avec dans chaque colonne
			# le signal median du canal 
#pData : data frame avec 2 colonnes obligatoire nomFichier Dye
#type : type de puce, agilent, nimbgene, gpr ou custum : (AG, NG, GPR, CUST) vecteur(variable) character
#namesGenes : nom des identifiants de g?nes vecteur character


#Verification des parametres d'entree :

	require(limma)
	if(missing(type))
		stop(" Type de puce manquantes ")
	if(! type == "AG" & ! type == "NG" & ! type == "GPR")
		stop("Type de puce non correcte : seul les types AG (agilent) NG (NimbelGene) et GPR (issue de GenePix) sont disponibles")
	
		if(missing(namesGenes))
		warning("Le nom pour les genes n'est pas donne. 
              Les colonnes GeneID et FeatureNum seront utilisees par defaut")
  
	dye<-length(unique(pData$Dye))
	print(dye)

	#Setup des arguments pour la creation de la matrice.
	# type de puce et noms des colones a extraire.
	other.col<-c()
	source<-c()
	if( type == "AG"){
		other.col<-c("FeatureNum")
		source="agilent"
		if(dye == 2){
			cols<-list(R="rMedianSignal",G="gMedianSignal",Rb="rBGMedianSignal",Gb="gBGMedianSignal")
		}else{
			cols<-list(R="gMedianSignal",G="gMedianSignal",Rb="gBGMedianSignal",Gb="gBGMedianSignal")
		}
		
	} else if (type == "NG"){
		source="generic"
		dye<-1
		cols<-list(R="PM",G="PM",Rb="MM",Gb="MM") #Un seul canal par fichier pair mono couleur
		other.col=c("PROBE_ID")
	
	} else if (type == "GPR"){
		source="genepix.median"
		col<-list(R = "F635 Median", G = "F532 Median", Rb = "B635 Median", Gb = "B532 Median")
		other.col<- c("")
	}

	#parametre du nom des echantillons
	namesEch<-c()
	if ( dye == 2 && (type == "AG" || type == "GPR" ) ){
		namesEch<-c(rownames(pData)[which(tolower(pData$Dye) == "cy5")],
                rownames(pData)[which(tolower(pData$Dye) == "cy3")])
		print(namesEch)
    
	}else {
		namesEch<-rownames(pData)
	}


	#Lecture des fichiers pour creer un objet expressionArray. (limma)
	RG<-read.maimages(unique(pData$nomFichiers),source=source,columns=cols,other.columns=other.col,path=pathDir)

		if(dye == 2){
			print( " OK dye 2")
      pDataCy5<-pData[which(tolower(pData$Dye)== "cy5"),]
	  print("Cy5")
	  print(pDataCy5)
      pDataCy3<-pData[which(tolower(pData$Dye)== "cy3"),]
	  print("Cy3")
	  print(pDataCy3)
      R<-RG$R
      G<-RG$G
	  print("R")
     print(colnames(R)) 
	  print("G")
     print(colnames(G)) 
      R<-R[,intersect(removeExt(pDataCy5$nomFichiers),colnames(R))]
      G<-G[,intersect(removeExt(pDataCy3$nomFichiers),colnames(G))]
	  print("R")
     print(colnames(R)) 
	  print("G")
     print(colnames(G)) 

			data<-cbind(R,G)
	print(dim(data))
			
		}else {
			print ("dye 1")
			data<-RG$G
		}
	#nom des echantillons
	print(dim(data))
	
	print(length(namesEch))
	print(namesEch)
	colnames(data)<-namesEch
	#noms des genes
	rownames(data)<-namesGenes

	return(data)
}

`read.cdt` <-
function(file){
	cdt<-read.delim(file, sep="\t")
	cdt<-cdt[,-1]
	cdt<-cdt[,-1]
	cdt<-cdt[,-2]
	cdt<-cdt[-1,]
	cdt<-cdt[-1,]
	row.names(cdt)<-cdt[,1]
	cdt<-cdt[,-1]
	cdt<-as.data.frame(cdt)
	return(cdt)
}

`read.matrixArray` <-
function(file){
	mat<-read.delim(file, sep="\t")
	row.names(mat)<-mat[,1]
	mat<-mat[,-1]
	return(mat)

}

