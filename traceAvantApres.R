`traceAvantApres` <-
function(increm, matOrdonne, matNorm, nomEchan, pngDir)
{
		nom_fichier = paste(pngDir,"images/",nomEchan[increm],"_","3-AvantApres",".png",sep="")
		png(filename=nom_fichier)
    plot(matOrdonne[,increm],matNorm[,increm], log='xy', ylab=nomEchan[increm],pch=19,,cex=0.8,cex.lab=1.5,cex.axis=1.5)
    dev.off()
}

