`create_pathway` <-
function(p){

dirA<-paste("./",p,"-01-stat-descriptive",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-paste("./",p,"-02-normalisation",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-paste("./",p,"-02-normalisation/images",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-paste("./",p,"-03-clusterAleatoire",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-paste("./",p,"-04-filtre",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-paste("./",p,"-05-student",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-paste("./",p,"-06-annotation",sep="")
if(!file.exists(dirA))
	dir.create(dirA)

dirA<-"rapport"
if(!file.exists(dirA))
  dir.create(dirA)
}

