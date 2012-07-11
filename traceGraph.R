`traceGraph` <-
function(increm, matData, ref, diagonal, lowessCurve=NULL,nomEchan,status,pngDir)
{
	nom_fichier = paste(pngDir,"images/",nomEchan[increm],"_",status,".png",sep="")
	png(filename=nom_fichier)
    plot(ref,matData[,increm],log='xy', ylab=nomEchan[increm],pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5 )
    if(!is.null(lowessCurve)){
        lines(ref,exp(lowessCurve[,increm]),col=2,lwd=2)
    }
    lines(diagonal,diagonal,col=3,lwd=2)
    dev.off()

}

