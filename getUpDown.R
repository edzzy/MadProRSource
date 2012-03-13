`getUpDown` <-
function(vPval,preFileName){
	vPval<-tolower(vPval)
	up<-vPval[which(vPval=="up")]
	down<-vPval[which(vPval=="down")]
	upName<-paste(preFileName,"-Up.txt",sep="")
	downName<-paste(preFileName,"-down.txt",sep="")
	write.table(up,upName,col.names=NA,sep="\t",quote=FALSE)
	write.table(down,downName,col.names=NA,sep="\t",quote=FALSE)
	

}

