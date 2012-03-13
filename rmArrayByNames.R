rmArrayByNames<-function(matnum_expre, vech_namesArray){

#--rmArrayByNames prend une entree une matrice d'expression et un vecteur de nom de d'echantillons(pas forcement identique)

#-- retourne une matrice d'expression sans les echantillons contenu dans le vecteur de nom d'echantillons

#-- matnum_expre : matrice d'expression
#-- vech_namesArray : vecteur contenant une partie du nom de(s) échantillons à retirer.

	matnum_expreTmp<-NULL	
	for(i in 1:length(vech_namesArray)){
		if(nrow(matnum_expre) == 1){
			ntmp<-rownames(matnum_expre)
			matnum_expreTmp<-t(as.data.frame(matnum_expre[,-grep(vech_namesArray[i],colnames(matnum_expre))]))
			rownames(matnum_expreTmp)<-ntmp
		}else{
			matnum_expreTmp<-matnum_expre[,-grep(vech_namesArray[i],colnames(matnum_expre))]
		}
		if(ncol(matnum_expreTmp)==0){
			messagew<-paste("aucune correspondace trouve entre ", vech_namesArray[i], " et ", colnames(matnum_expre))
			warning(messagew)
		}else{
			matnum_expre<-matnum_expreTmp
		} 
	}
		return(matnum_expre)
}
