`graphMmobile` <-
function(filename,value,seuil=NULL,pas="",title=""){

	if(pas==""){
		pas <- length(value)*0.7/100
		pas<-round(pas,0)
		print(pas)
	}
	curveMobile<-rep(1/pas,pas)
	fil<-filter(value,curveMobile)
	png(filename=filename,width = 1300, height = 900, bg = "white", res = NA )
	par(mar=c(0,0,0,0),oma=c(0,0,0,0),mgp=c(0,0,0))
	plot(value,pch=19,cex=0.8,cex.lab=1.5,cex.axis=3,col="slateblue",bty="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(1,length(value)),ylim=range(value,na.rm=TRUE),main=title)
	lines(fil,lwd=5,col="orange2")
	axis(side=2,tcl=0.5,label=TRUE,pos=c(0,0),cex.axis=3)
	if (! is.null(seuil)){
	abline(h=seuil,lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col="red")
	}
	dev.off()
}

