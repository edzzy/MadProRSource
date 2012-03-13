`createMatrix2png` <-
function(frameFac){
#DESCRIPTION : Change les noms des levels d'un tableau de parametres d'echantillons en numeros.
#INPUT : matrix ou data.frame avec en colonne le nom des echantillon, en ligne les parametres
#OUTPUT : Renvoi une matrix avec en premier colonne le nom des facteur (levels) du parametre puis les différents levels
	if(missing(frameFac))
		stop("argument frameFac est manquant")

	frameFac<-as.matrix(frameFac)
	p<-1
	frameFacOR<-frameFac
	#Boucle qui subsitue un level par un numeros.
	for( i in 1:nrow(frameFac)){
		nl<-nlevels(as.factor(frameFac[i,]))
		l<-levels(as.factor(frameFac[i,]))
		l<-sort(l,TRUE)
		for( j in 1:nl){
			frameFac<-sub(l[j],p,frameFac)
			p<-p+1
		}
	}
	#verifie que frameFac n'est pas un vecteur ### partie obsolete	
	if(is.null(dim(frameFac))){
		nr<-c(l[1])
		for(i in 2:length(l)){
			nr<-paste(nr,l[i],sep="-")
		}
		frameFac<-c(nr,frameFac)
	#creer un vecteur avec le nom des parametres (concatenation de tous les niveaux du parametre)
	}else{
		for(p in 1:nrow(frameFac)){
			pl<-levels(as.factor(frameFacOR[p,]))
			nnr<-c(pl[1])
			for(i in 2:length(pl)){
				nnr<-paste(nnr,pl[i],sep="-")
			}
			if(p==1){
				nr<-nnr
			}else{
				nr<-c(nr,nnr)
			}
		}
		frameFac<-cbind(nr,frameFac)
	}
	#Remplace les NA par 0
	frameFac<-replace(frameFac,which(is.na(frameFac)),0)		
	return(frameFac)
}

