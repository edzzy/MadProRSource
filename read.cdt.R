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

