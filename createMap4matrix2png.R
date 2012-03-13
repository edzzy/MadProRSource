`createMap4matrix2png` <-
function(frameFac){
	#DESCRIPTION : Creer une matrice d'association de couleurs qui sera utilise par le programe unix matrix2png
	#INPUT : matrix des parametre des echantillons
	#OUTPUT : matrix qui correspond a la map des couleurs, une colonne value (entier) 
														#une avec la couleur (RGB) 
														#et une avec le nom du parametre
	if(missing(frameFac))
		stop("argument frameFac manquant")
	if(  class(frameFac) != "matrix" & class(frameFac) != "data.frame")
		stop("argument frameFac n'est ni de 'class matrix' ni de 'class data.frame'")

	frameFac<-as.matrix(frameFac)
	n<-nlevels(as.factor(frameFac))
	nColor<-seq(1:n)
	mp<-rainbow(n);
	mp<-sample(mp,n)
	n<-n+nrow(frameFac)
	nal<-seq(1:n)
	mapF<-data.frame()
	for( i in 1:nrow(frameFac)){
		nl<-nlevels(as.factor(frameFac[i,]))
		l<-levels(as.factor(frameFac[i,]))
		n<-sort(l,TRUE)
		mt<-mp[1:nl]
		mp<-mp[-1:-nl]
    	cl<-t(col2rgb(mt))
    	color<-paste(cl[,1],cl[,2],cl[,3],sep=":")	
		color<-c("255:255:255",color)
		l<-c("**",n)
		n<-nl+1
		value<-nal[1:nl]
		value<-c(nal[length(nal)],value)
		nal<-nal[-1:-nl]
		nal<-nal[-length(nal)]
		map<-cbind(value,color,l)
		mapF<-rbind(mapF,map)
	}
	return(mapF)
}

