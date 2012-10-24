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
