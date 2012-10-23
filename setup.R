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

