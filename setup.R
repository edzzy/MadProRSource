`create_pathway` <-function(p){

dirA<-paste("./",p,"-01-stat-descriptive/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(dirA)
colnames(treepath)[ncol(treepath)]<-"stat"

dirA<-paste("./",p,"-02-normalisation/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"normalisation"


dirA<-paste("./",p,"-02-normalisation/images/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"normalisationImage"

dirA<-paste("./",p,"-03-clusterAleatoire/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"clusterAlea"

dirA<-paste("./",p,"-04-filtre/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"filtre"

dirA<-paste("./",p,"-05-student/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"student"

dirA<-paste("./",p,"-06-annotation/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"annotation"

dirA<-"rapport/"
if(!file.exists(dirA))
  dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"rapport"

dirA<-paste("./",p,"-07-resultat/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultat"

dirA<-paste("./",p,"-07-resultat/",p,"-01-matrice/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatMat"


dirA<-paste("./",p,"-07-resultat/",p,"-02-cluster/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatClust"

dirA<-paste("./",p,"-07-resultat/",p,"-02-cluster/listes/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatClustList"

dirA<-paste("./",p,"-07-resultat/",p,"-02-cluster/annotations/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatClustAnnot"


dirA<-paste("./",p,"-07-resultat/",p,"-03-genDiff/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatGeneDiff"

dirA<-paste("./",p,"-07-resultat/",p,"-03-genDiff/listes/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatGeneDiffListe"

dirA<-paste("./",p,"-07-resultat/",p,"-03-genDiff/annotations/",sep="")
if(!file.exists(dirA))
	dir.create(dirA)
treepath<-cbind(treepath,dirA)
colnames(treepath)[ncol(treepath)]<-"resultatGeneDiffAnnot"


return(as.data.frame(treepath))
}

getPattern<-function(file){

	pattern<-read.delim(file,skip=1,nrow=1,header=TRUE)$FeatureExtractor_PatternName
	
	return(as.character(pattern))
	
}

getInfo<-function(file){

	design<-getPattern(file)
	infoName<-paste(system.file("extdata",package="MadProDev"),"/",design,sep="")
	if(file.exists(infoName)){
		info<-read.delim(infoName,header=TRUE,row.names=1)
		return(info)
	}else{
		message<-paste("Pas de fichier d'annotation défini pour ", design," un fichier à été creer mettre à jour Le package",sep="")
		info<-createAgilentAnnotation(file)
		warnings(message)
		return(info)	
	}
}

getPuceInfo<-function(design){
	puceName<-paste(system.file("extdata",package="MadProDev"),"/puce",sep="")
	puce<-read.delim(puceName)
	info<-puce[which(puce[,1] == design),]
	if(nrow(info) != 0){
		return(info)
	}else{
		return(NULL)	
	}
}
createAgilentAnnotation<-function(file){
	info<-data.frame()
	if(!file.exists(file)){
			mess<-paste("file : ", file, " doesn't exists",sep="")
			stop(mess)
		}

	tmp<-read.delim(file,skip=9,header=TRUE)
if(!is.null(tmp$FeatureNum)){
	info<-cbind(tmp$FeatureNum)
	colnames(info)<-c("FeatureNum")
}else{
	stop("Champs FeatureNum n'existe pas")
	}
if(!is.null(tmp$accessions))
	info<-cbind(info,as.character(tmp$accessions))
	colnames(info)[ncol(info)]<-"accessions"
if(!is.null(tmp$chr_coord))
	info<-cbind(info,as.character(tmp$chr_coord))
	colnames(info)[ncol(info)]<-"chr_coord"
if(!is.null(tmp$Start))
	info<-cbind(info,as.character(tmp$Start))
	colnames(info)[ncol(info)]<-"Start"
if(!is.null(tmp$Sequence))
	info<-cbind(info,as.character(tmp$Sequence))
	colnames(info)[ncol(info)]<-"Sequence"
if(!is.null(tmp$ProbeUID))
	info<-cbind(info,as.character(tmp$ProbeUID))
	colnames(info)[ncol(info)]<-"ProbeUID"
if(!is.null(tmp$ControlType))
	info<-cbind(info,as.character(tmp$ControlType))
	colnames(info)[ncol(info)]<-"ControlType"
if(!is.null(tmp$ProbeName))
	info<-cbind(info,as.character(tmp$ProbeName))
	colnames(info)[ncol(info)]<-"ProbeName"
if(!is.null(tmp$GeneName)){
	info<-cbind(info,as.character(tmp$GeneName))
	colnames(info)[ncol(info)]<-"GeneName"
}else{
	stop("Champs GeneName n'existe pas")
}
if(!is.null(tmp$SystematicName))
	info<-cbind(info,as.character(tmp$SystematicName))
	colnames(info)[ncol(info)]<-"SystematicName"
if(!is.null(tmp$Description ))
	info<-cbind(info,as.character(tmp$Description))
	colnames(info)[ncol(info)]<-"Description"

madId<-paste(tmp$FeatureNum,"//",tmp$GeneName,sep="")
rownames(info)<-madId
pattern<-getPattern(file)
write.table(info,pattern,row.names=FALSE,sep="\t",quote=FALSE)

	return(info)

}


verifParam<-function(pSetupFile){
	
	CheckError=FALSE
	nError<-0

	if(!file.exists(pSetupFile)){
	  stop("Le fichier", pSetupFile," n'est pas present")
	  CheckError=TRUE
	}

	pSetup<-read.delim(pSetupFile,row.names=1,header=FALSE,stringsAsFactors=FALSE,sep="\t")
	pSetup<-t(pSetup)
	pSetup<-as.data.frame(pSetup)

	if(nrow(pSetup) != 1){
		stop("Mauvais formatage du fichier ",pSetupFile,". Nombre de colone > 1. Veuillez vérifier si il n'existe pas de tabulations parasite en fin de lignes.") 
		cat("\n",date(),"\n",file="error.log",append=TRUE)
		CheckError=TRUE
	}
	#-Import/Verification du nom de projet
	
	if(is.null(pSetup$Projet)){
		CheckError=TRUE
		cat("Le nom du projet est manquant dans le fichier : ", pSetupFile,"\n",file="error.log",append=TRUE)
		nError <- nError +1
	}
	
	
	
	#Import/Verification du type d'array agilent=AG ou gpr=GPR nimbelgen=NG
	if(is.null(pSetup$typeArray)){
		CheckError=TRUE
		cat("Le type d'array n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
	}else{
		typeArray<- as.character(pSetup$typeArray) 
		typeArray<-toupper(typeArray)
		if(typeArray != "AG" & typeArray != "GPR" & typeArray != "NG"){
			CheckError=TRUE
			cat("Le type d'array n'est pas le bon dans le fichier : ", pSetupFile," indiquer AG pour agilent, NG pour nimbelGen ou GPR pour genepix\n", file="error.log",append=TRUE)
			nError <- nError +1
		}
	}
	#nombre de sondes pour la test matrice aleatoire.
	
	
	if(is.null(pSetup$fileEch)){
			CheckError=TRUE
			cat("Le parametre fileEch n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
			nError <- nError +1
	}else{
		fileEch<-as.character(pSetup$fileEch)
		if(!file.exists(fileEch)){
			CheckError=TRUE
			cat("Le fichier d'annotation des echantillons ",fileEch," est introuvable.\n", file="error.log",append=TRUE)
			nError <- nError +1
		}
	}
	###
	
	
	if(!is.null(pSetup$dirFile)){
		dirFile<-as.character(pSetup$dirFile)
		if(!file.exists(dirFile)){
			CheckError=TRUE
			cat("Le repertoire : ",dirFile," est introuvable.\n", file="error.log",append=TRUE)
			nError <- nError +1
		}
	}else{
			CheckError=TRUE
			cat("Le parametre dirFile n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
			nError <- nError +1
	}
	
	###
	if(!is.null(pSetup$nval)){
		nval<-as.numeric(as.character(pSetup$nval))
		if(nval < 0 | nval > 100){
			CheckError=TRUE
			cat("Le parametre nval doit être compris entre 0 et 100 il s'agit d'un pourcentage.\n", file="error.log",append=TRUE)
			nError <- nError +1
		}
	}else{
			CheckError=TRUE
			cat("Le parametre nval n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
			nError <- nError +1
	}
	#nbclasses<-as.numeric(pSetup$nclasses)
	
	if(!is.null(pSetup$filterParam)){
		filterParam<-as.numeric(as.character(pSetup$filterParam))
	}else{
			CheckError=TRUE
			cat("Le parametre filterParam n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
			nError <- nError +1
	}
	
	
	
	if(is.null(pSetup$comparFile)){
		comparaison<-NULL
	}else{
		comparFile<-as.character(pSetup$comparFile) 
		if(!file.exists(comparFile))
			stop("le fichier", comparFile, " est introuvable") 
			comparaison<-read.delim(comparFile,header=FALSE, sep="\t")
	}
	
	
	if(CheckError){
		stop("Il y a eu ",nError," erreurs lors du setup du pipeline veillez regarder le fichier error.log pour les corriger" )
	}
	print ("Fin Setup de l'analyse")
	
	
	
	return(CheckError)	
	
}

getParam<-function(pSetupFile){
	
	CheckError=FALSE

	if(!file.exists(pSetupFile)){
	  stop("Le fichier", pSetupFile," n'est pas present")
	  CheckError=TRUE
	}

	pSetup<-read.delim(pSetupFile,row.names=1,header=FALSE,stringsAsFactors=FALSE,sep="\t")
	pSetup<-t(pSetup)
	pSetup<-as.data.frame(pSetup)

	if(nrow(pSetup) != 1){
		stop("Mauvais formatage du fichier ",pSetupFile,". Nombre de colone > 1. Veuillez vérifier si il n'existe pas de tabulations parasite en fin de lignes.") 
		cat("\n",date(),"\n",file="error.log",append=TRUE)
		CheckError=TRUE
	}
	
	return(pSetup)
}

getProjet<-function(pSetup){
	return(as.character(pSetup$Projet))
}
getTypeArray<-function(pSetup){
	return(as.character(pSetup$typeArray))
}
getDirFile<-function(pSetup){
	return(as.character(pSetup$dirFile))
}
getFileEch<-function(pSetup){
	return(as.character(pSetup$fileEch))
}
getNormType<-function(pSetup){
	if(!is.null(pSetup$NormType)){
		return(as.character(pSetup$NormType))
		
	}else{
		return("L")
	}
}
getNval<-function(pSetup){
#Fucking typage dans R!!!!!!
	return(as.integer(as.character(pSetup$nval)))
}
getComparFile<-function(pSetup){
	return(as.character(pSetup$comparFile))
}
getFilterParam<-function(pSetup){
	return(as.numeric(pSetup$filterParam))
}
getAlea<-function(pSetup){
	if(!is.null(pSetup$alea)){
		return(as.integer(as.character(pSetup$alea)))
	}else{
		return(20)		
	}	
}
