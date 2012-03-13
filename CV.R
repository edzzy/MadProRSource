CV<-function(matrice){
    if(missing(matrice)){              #test si la matrice est presente
        stop("L'argument matrice est manquant")
    }
    matrice<-as.matrix(matrice)
    ecartType<-apply(matrice,1,sd)                                                                                           
    moyenne<-apply(matrice,1,mean)
    CV<-ecartType/moyenne
    return(CV)
}

