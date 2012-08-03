`create_boxplot` <-
function (data,output_name)
##fonction pour créer des boites à moustaches
{
  bottomarg = nchar(max(colnames(data))) #nombre de ligne pour la marge du bas
  jpeg(filename = output_name, width = 1300, height = 900, quality = 100, bg = "white", res = NA)
  par(mar=c(bottomarg + 5,5,3,3))
  boxplot(log(as.data.frame(data)), col=rainbow(n=20), las="2",cex.lab=1.5,cex.axis=1.5)
  dev.off()
}

`create_correlation_median` <-
function (data,output_name,seuil=0.8)
##Correlation par rapport au profil median
{
  bottomarg = nchar(max(colnames(data))) #nombre de ligne pour la marge du bas
  prof_med = apply(data,1,median)
  correl=apply(data,2,cor,prof_med)
  pcol<-rep(1,ncol(data))
  badCor<-which(correl<seuil)
  pcol[badCor]=2
  jpeg(filename = output_name, width = 1300, height = 900, quality = 100, bg = "white", res = NA)
  par(mar=c(bottomarg +5,5,3,3))
  plot(correl, type="l",xlab="", ylab="Correlation par rapport au profil median", ylim=c(0,1),cex.lab=1.5,cex.axis=1.5,xaxt="n")
  points(correl, col=pcol,xlab="",pch=19,cex=1.5, ylab="Correlation par rapport au profil median", ylim=c(0,1),cex.lab=1.5,cex.axis=1.5,xaxt="n")
  axis(1,1:dim(data)[2],labels=colnames(data),las="2",cex.axis=1.5)
  abline(h=seuil,col="red")
  abline(v=badCor,col="green")	
  dev.off()
	return(names(badCor))
}

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

`create_stats_des` <-
function (data,output_name,minMax=TRUE){
##Statistiques descriptives
##DESCRIPTION
#fonction qui tracent les valeurs particulieres pour chaque echantillons d'une matrice d'expression
#dans un fichier image (png)
##ARG
#data est une matrice de donnees d'expression de type data.frame ou matrix mais dont le type dois etre numerique ou double.
#output est le nom du fichier de sortie de l'image : chaine de caractere
#minMax est un boolen qui permet ou non de tracer les valeurs max et min ainsi que les 10eme plus grande et plus petite valeur
#par defaut minMax est TRUE
##RETURN
#cette fonction ne retourne aucune valeur.

    #Je tri les valeurs par colonnes en ordre croissant
	print(class(data))
	print(typeof(data))
    dataSortedBySample=apply(data,2,sort)
	print(class(dataSortedBySample))
	print(typeof(dataSortedBySample))
    nbgenes = dim(data)[1]
	print(nbgenes)
    nbech = dim(data)[2]
    bottomarg = nchar(max(colnames(data))) #nombre de ligne pour la marge du bas
    #1er, 10e ... plus grande valeur
    v1=dataSortedBySample[nbgenes,]
    v10=dataSortedBySample[nbgenes-9,]
    v100=dataSortedBySample[nbgenes-99,]
    if(nbgenes>999){v1000=dataSortedBySample[nbgenes-999,]}
    if(nbgenes>9999){v10000=dataSortedBySample[nbgenes-9999,]}
    if(nbgenes>19999){v20000=dataSortedBySample[nbgenes-19999,]}
    v1p=dataSortedBySample[1,]
    v10p=dataSortedBySample[1+9,]
    v100p=dataSortedBySample[1+99,]
    if(nbgenes>999){v1000p=dataSortedBySample[+999,]}
    if(nbgenes>9999){v10000p=dataSortedBySample[1+9999,]}
    if(nbgenes>19999){v20000p=dataSortedBySample[1+19999,]}

    x = c(1,nbech+1) #x pour le plot invisible
    rangev = range(v1,v1p) #pour y : 'range' returns a vector containing the minimum and maximum of all the given arguments.
    max = max(v1)
    milieu = max/2 #milieu de l'axe des y 	

    
    jpeg(filename = output_name, width = 1300, height = 900, quality = 100, bg = "white", res = NA)
    par(mar=c(bottomarg +5,5,3,25))
    plot(x,rangev, log="y", type="n",xaxt="n", xlab="", ylab="Intensity",cex.lab=1.5,cex.axis=1.5)
    # type="n" '"n"' for no plotting
	if(minMax){
		lines(v1, col="black")
		lines(v10, col="red")
		legend=c("valeur max","10eme plus grande valeur","100eme plus grande valeur")
		col=c("black","red","blue")
	}else{
		legend=c("100eme plus grande valeur")
		col=c("blue")
	}


	lines(v100, col="blue")


    lty<- rep(1,4)
	if(exists("v1000")){
		lines(v1000, col="green")
		legendtmp<-c("1000 plus grande valeur")
    	coltmp<-c("green")
    	ltytmp<-1

		legend<-c(legend,legendtmp)
    	col<-c(col,coltmp)
    	lty<-c(lty,ltytmp)

	}
    if(exists("v10000")){
    	lines(v10000, col="orange")
    	legendtmp<-c("10000 plus grande valeur")
    	coltmp<-c("orange")
    	ltytmp<-1
    
    	legend<-c(legend,legendtmp)
    	col<-c(col,coltmp)
    	lty<-c(lty,ltytmp)
    }
    if(exists("v20000")){
    	lines(v20000, col="red")
    	legendtmp<-c("20000 plus grande valeur")
    	coltmp<-c("red")
    	ltytmp<-1
    
    	legend<-c(legend,legendtmp)
    	col<-c(col,coltmp)
    	lty<-c(lty,ltytmp)
    }
    
	if(minMax){
		lines(v1p, col="blue",lty=5)
		lines(v10p, col="orange",lty=5)
	}
	
    lines(v100p, col="green",lty=5)

    legendp<-NULL	
	if(exists("v1000p")){
		lines(v1000p, col="red",lty=5)
		legendp<-c("1000eme petite valeur")
        coltmp<-c("red")
        ltytmp<-5                                            
       col<-c(col,coltmp)
       lty<-c(lty,ltytmp)


	}
    if(exists("v20000p")){
    	lines(v20000p, col="grey",lty=5)
    	legendp<-c("20000eme petite valeur")
        coltmp<-c("grey")
        ltytmp<-5                                            
       col<-c(col,coltmp)
       lty<-c(lty,ltytmp)
    }

    if(exists("v10000p")){
      	lines(v10000p, col="purple",lty=5)
      	legendp<-c(legendp,"10000eme petite valeur")
        coltmp<-c("purple")
        ltytmp<-5
                                                 
       col<-c(col,coltmp)
       lty<-c(lty,ltytmp)
	 }
	if(exists("v1000p")){
		lines(v1000p, col="red",lty=5)
		legendp<-c("1000eme petite valeur")
        coltmp<-c("red")
        ltytmp<-5                                            
       col<-c(col,coltmp)
       lty<-c(lty,ltytmp)


	}

	if(minMax){
		colp<-c("green","orange","blue")
		legendp<-c(legendp, "100eme petite valeur", "10eme petite valeur", "valeur min")	
	}else{
		colp<-c("green")
		legendp<-c(legendp,"100eme petite valeur")	
	}
    legend<-c(legend,legendp)
    col<-c(col,colp)
    ltyp<-rep(5,4)
    lty<-c(lty,ltyp)
	 
    axis(1,1:nbech,labels=colnames(data),las="2",cex.axis=1.5)
    par(xpd=TRUE)	
    
    legend(nbech+1.5,milieu, yjust=1, legend=legend,lwd=1,cex=1.5,col=col,lty=lty)
    dev.off()
}

graphlevelExpress<-function(expres,filename=".",title="",col=c(1),seuil=0,pch=c(19)){

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

			lines(unlist(expres[i,]),col=1,main=title,type="l")
			points(unlist(expres[i,]),col=col,main=title,pch=pch)

	}
abline(h=seuil,col="red")
axis(1,1:ncol(expres),labels=colnames(expres),las="2",cex.axis=1.5)
dev.off()

}
`traceAvantApres` <-
function(increm, matOrdonne, matNorm, nomEchan, pngDir)
{
		nom_fichier = paste(pngDir,"images/",nomEchan[increm],"_","3-AvantApres",".png",sep="")
		png(filename=nom_fichier)
    plot(matOrdonne[,increm],matNorm[,increm], log='xy', ylab=nomEchan[increm],pch=19,,cex=0.8,cex.lab=1.5,cex.axis=1.5)
    dev.off()
}

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

