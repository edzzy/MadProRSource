`read_Agilent` <-
function(namesArray,files,type,namesGenes,dye=2){
#Description : La fonction renvoie une matrice d'expression en lisant les fichiers
			# issus de logiciel d'extraction d'image avec dans chaque colonne
			# le signal median du canal 
#namesArray : nom des echantillons. data.frame avec deux colonnes : nR et nG
###files nom des fichiers de sorties des logiciels d'extraction d'image de scanner 
	#(feature extraction, nimblScan, genepix) vecteur character
#type : type de puce, agilent, nimbgene, gpr ou custum : (AG, NG, GPR, CUST) vecteur(variable) character
#namesGenes : nom des identifiants de gènes vecteur character
#dye nombre de canal , par défaut 2 (le plus courant)

#Verification des parametres d'entree :

	require(limma)
	if(missing(type))
		stop(" Type de puce manquantes ")
	if(! type == "AG" & ! type == "NG" & ! type == "GPR")
		stop("Type de puce non correcte : seul les types AG (agilent) NG (NimbelGene) et GPR (issue de GenePix) sont disponibles")
	if(missing(files))
		stop("Noms des fichiers d'expression manquant") 
	if(missing(namesArray))
		stop("Le nom pour les echantillons est manquant")
	if(missing(namesGenes))
		warning("Le nom pour les genes n'est pas donne. 
              Les colonnes GeneID et FeatureNum seront utilisees par defaut")
	if( ! dye == 1 & ! dye == 2){
		stop("Le dye doit etre 1 ou 2")
	}
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
		namesEch<-c(unlist(as.character(namesArray$nR)),unlist(as.character(namesArray$nG)))
	}else {
		namesEch<-unlist(as.character(namesArray))
	}

	#Lecture des fichiers pour creer un objet expressionArray. (limma)
	RG<-read.maimages(files,source=source,columns=cols,other.columns=other.col)

		if(dye == 2){
			data<-cbind(RG$R,RG$G)
		}else {
			data<-RG$G
		}
	#nom des echantillons
	colnames(data)<-namesEch
	#noms des genes
	rownames(data)<-namesGenes

	return(data)
}

