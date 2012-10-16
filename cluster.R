detectCluster<-function(pval,seuil=1,seuilCluster=150,pas=200,comparaison_vect=FALSE,mat=FALSE,f=FALSE,out_name="detecClust.png" ){
	require(genefilter)
	require(MadProDev)
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
		 print("TOTO")
		f<-as.matrix(f)
		mat<-as.matrix(mat)
		if (comparaison_vect != FALSE){
		comparaison_name <-paste(comparaison_vect[1],comparaison_vect[2],sep="-Vs-")
		m1<-meanByFact(mat,f,comparaison_vect[1])
		m2<-meanByFact(mat,f,comparaison_vect[2])
		tabmean<-cbind(m1,m2,pval)
		value<-apply(tabmean,1,graphClustPval)
		}
			
		fil<-filter(value,curveMobile)


	coordClust<-fusionCluster(selectBegin,selectEnd)
	jpeg(filename = out_name, width = 1300, height = 900, quality = 100, bg = "white", res = NA)
	par(mar=c(0,0,0,0),oma=c(0,0,0,0),mgp=c(0,0,0))
	plot(value,pch=19,cex=0.8,cex.lab=1.5,cex.axis=3,col="slateblue",bty="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(1,length(value)),ylim=range(value,na.rm=TRUE))
	lines(fil,lwd=5,col="orange2")
	axis(side=2,tcl=0.5,label=TRUE,pos=c(0,0),cex.axis=3)
	abline(h=seuil,lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col="darkgreen",lty=2)
	abline(h=-seuil,lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col="darkgreen",lty=2)
	abline(h=0,lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col="darkred")
	abline(v=coordClust[,1],lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col=c(1:nrow(coordClust)))
	abline(v=coordClust[,2],lwd=2,pch=19,cex=0.8,cex.lab=1.5,cex.axis=1.5,col=c(1:nrow(coordClust)))
	dev.off()


}
extendBegin<-function(begin,sensGraph){
	
	test<-FALSE
	sens<-sensGraph[begin]
	while(test == FALSE){
		indiceValle<-begin -1
		nbegin<-max(which(sens != sensGraph[1:indiceValle]))
		if(begin == 51){ 
			test<-TRUE
		}else{
		
			tmpbegin<-nbegin-50
			test<- sum(sensGraph[tmpbegin:nbegin]) < -10
			begin<-nbegin-1
		}
		message<-paste(test," ",begin,sep="")
#		print(message)
	}
	nbegin<-nbegin+1
	return(nbegin)



}
extendEnd<-function(end,sensGraph){

	test<-FALSE
	sens<-sensGraph[end]


	while(test == FALSE){
		indiceDescente<-end+1
		nend<-min(which(sensGraph[end] != sensGraph[indiceDescente:length(sensGraph)]))	
		nend<-length(sensGraph[1:end]) +nend
		tmpend<- nend + 50
		test<-sum(sensGraph[nend:tmpend]) < -10
		end<-nend+1
		}	

	return(nend)


}
fusionCluster<-function(begin,end){

	interXY<-0
	clusterTotal<-length(begin)
	finaleBegin<-NULL
	finaleEnd<-NULL				

	while(length(begin) != 1){		
		b1<- begin[1]
		e1<- end[1]
		b2<- begin[2]
		e2<- end[2]

		x<-seq(b1:e1) + b1 - 1
		y<-seq(b2:e2) + b2 - 1
		interXY<-intersect(x,y)  
		if(length(interXY) == 0){
			finaleBegin<-c(finaleBegin,b1)
			finaleEnd<-c(finaleEnd,e1)
			begin<-begin[-1]
			end<-end[-1]
		}else{
			rXY<-range(union(x,y))
			begin<-begin[-1]
			end<-end[-1]
			begin[1]<-rXY[1]
			end[1]<-rXY[2]

		} 
	}

	finaleBegin<-c(finaleBegin,begin)	
	finaleEnd<-c(finaleEnd,end)	

	coordClust<-cbind(finaleBegin,finaleEnd)
	print(finaleBegin)
	print(finaleEnd)
	return(coordClust)
}
