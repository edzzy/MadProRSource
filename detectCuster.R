detectCluster<-function(pval,seuil=1,seuilCluster=150,pas=200 ){
	require(genefilter)
	value<- -log10(pval)
	curveMobile<-rep(1/pas,pas)
	fil<-filter(value,curveMobile)
	select<- fil > seuil
	selectm1<-select[-1]
#indice de début et de fin, +1 pour le début car le TRUE et sur le "brin" de longeur n-1
	begin<- which(select == FALSE & selectm1 == TRUE)
	end<- which(select == TRUE & selectm1 == FALSE)
	#sens graphe est de longeur n-1 donne le signe de l'indice n+1 (indice 1 de sens graphe donne le sens de l indice 2 de value
	sensGraph<-sign(diff(fil))
	
	#Preparation à l'extension des pics : Recherche de la prochaine vallé pour le début et la fin
		
	
	newBegin<-sapply(begin,extendBegin,sensGraph)
	newEnd<-sapply(end,extendEnd,sensGraph)

	tailleCluster<-newEnd - newBegin

	selectBegin<-newBegin[which(tailleCluster >= seuilCluster)]
	selectEnd<-newEnd[which(tailleCluster >= seuilCluster)]
	
	coordClust<-fusionCluster(selectBegin,selectEnd)
	par(mar=c(0,0,0,0),oma=c(0,0,0,0),mgp=c(0,0,0))
	plot(value,pch=19,cex=0.8,cex.lab=1.5,cex.axis=3,col="slateblue",bty="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(1,length(value)),ylim=range(value,na.rm=TRUE))
	lines(fil,lwd=5,col="orange2")
	axis(side=2,tcl=0.5,label=TRUE,pos=c(0,0),cex.axis=3)
	abline(h=seuil,lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col="red")
	abline(v=coordClust[,1],lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col=c(1:nrow(coordClust)))
	abline(v=coordClust[,2],lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col=c(1:nrow(coordClust)))


}
