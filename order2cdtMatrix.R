`order2cdtMatrix` <-
function(matlowess, matcdt){
	#pour securite on type les deux element en data.frame
	matlowess<-as.data.frame(matlowess)
	matcdt<-as.data.frame(matcdt)
	
	#On ajoute le nom des genes en colonne 1 pour les deux "matrices"
	matlowess<-cbind(row.names(matlowess), matlowess)
	matcdt<-cbind(row.names(matcdt), matcdt)
	
	#on creer un vecteur de la taille du nombre de genes qui va servir d'ordre pour le clustering
	n<-dim(matcdt)[1]
	ngene<-c(1:n)
	
	#on ajoute ce vecteur a la matrice cdt
	matcdt<-cbind(ngene,matcdt)
	
	#on trie les deux matrices par le nom des genes 
	matcdt<-matcdt[order(matcdt[,2]),]
	matlowess<-matlowess[order(matlowess[,1]),]

	#on ajoute le vecteur d'ordre de clustering a la matrice non clusterise
	matlowess<-cbind(matcdt[,1],matlowess)
	
	#on ordonne dans l'ordre du clustering
	matlowess<-matlowess[order(matlowess[,1]),]


	#on nettoie tout (on enleve l'ordre du clustering et le nom des genes colonne 1 et 2
	orderMatrix<-matlowess[,-1]
	orderMatrix<-orderMatrix[,-1]	

	return (orderMatrix)
}

