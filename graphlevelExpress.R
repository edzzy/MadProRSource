graphlevelExpress<-function(expres,filename=".",title="",col=c(1),seuil=0){

	expres<-as.matrix(expres)	
	rangeExpre<-range(expres)
	min<-rangeExpre[1]
	max<-rangeExpre[2]
    x = c(1,ncol(expres)+1) #x pour le plot invisible

    bottomarg = nchar(max(colnames(expres))) #nombre de ligne pour la marge du bas

	jpeg(filename=filename, width = 1300 , height = 900, quality = 100,res=NA, bg= "white")
    par(mar=c(bottomarg +5,5,10,5))
	plot(x,rangeExpre,type="n",xaxt="n",xlab="n",ylab="intensity",cex.lab=1.5,cex.axis=1.5,main=title)
	for(i in 1:nrow(expres)){

			lines(unlist(expres[i,]),col=col,main=title,type="o",pch=19)

	}
abline(h=seuil,col="red")
axis(1,1:ncol(expres),labels=colnames(expres),las="2",cex.axis=1.5)
dev.off()

}
