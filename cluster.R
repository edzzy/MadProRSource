`clusterbyPair` <-
function(mat,frameFac,nameCl="cluster",order=FALSE){
	#clustering entre chaque paramétre
	nc<-0
	nct<-0
	nnct<-0
	for (i in 1:nrow(frameFac)){
		nl<-nlevels(as.factor(frameFac[i,]))
		tnl<-(nl*nl -nl) / 2
		nnct<-nnct+tnl
		print(nnct)
			
	}
	for (i in 1:nrow(frameFac)){
		nl<-nlevels(as.factor(frameFac[i,]))
		if(nl >=3){
			nlm<-nl-1
			le<-levels(as.factor(frameFac[i,]))
		
			for(j in 1:nlm){
				jp<-j+1
				for(k in jp:nl){
					tmpMat<-mat[,which(frameFac[i,]==le[j] | frameFac[i,]==le[k])]
					tmpFrame<-frameFac[,which(frameFac[i,]==le[j] | frameFac[i,]==le[k])]
					prefName<-paste(le[j],le[k],sep="-")
					suffName<-paste(nameCl,"txt",sep=".")
					nameMat<-paste(prefName,suffName,sep="-")
					writeMatrice(tmpMat,nameMat)

					suffName<-paste(nameCl,"frame",sep="-")
					suffName<-paste(suffName,"txt",sep=".")
					nameFrame<-paste(prefName,suffName,sep="-")
					write.table(tmpFrame,nameFrame,col.names=NA,sep="\t",row.names=TRUE);
					if( nc < 3  &  nct != nnct-1 ){
						symb="&"
						ech="1"
			#			commandCluster<-paste(" cluster -f ",nameCl," -l  -cg m -g 1 -e 1  -m c",sep="")
			#			system(commandCluster)
					}else{
						nc<-0
						symb=""
						ech="1"
			#	commandCluster<-paste(" cluster -f ",nameCl," -l  -cg m -g 1 -e 1  -m c",sep="" & )
			#	system(commandCluster)		
						
					}
					if(order){
				#trie des echantillons par classes dans la matrice et dans le tableau de paramètres
						tmpFrame<-tmpFrame[,names(sort(tmpFrame[i,]))]
						tmpMat<-tmpMat[,colnames(tmpFrame)]
				#nom des fichiers matrice et tableau parametres
						suff<-"order"	
						prefName<-paste(le[j],le[k],suff,sep="-")
						suffName<-paste(nameCl,"txt",sep=".")
						nameMat<-paste(prefName,suffName,sep="-")
						writeMatrice(tmpMat,nameMat)

						suffName<-paste(nameCl,"frame",sep="-")
						suffName<-paste(suffName,"txt",sep=".")
						nameFrame<-paste(prefName,suffName,sep="-")
						write.table(tmpFrame,nameFrame,col.names=NA,sep="\t",row.names=TRUE);
						ech="0"
					}	
					nc<-nc+1			
					nct<-nct+1			
					lo<-paste("param",i,"levels",le[j],"level",le[k],"nombre clust en cours",nc ,"nb clust total",nct,"/",nnct,"*",sep=":  ")
					commandCluster<-paste(" cluster -f ",nameMat," -l  -cg m -g 1 -e ",ech,"  -m c ", symb ,sep="" )
			
					print(lo)	
					print(commandCluster)
				#	system(commandCluster)		
				}
			}
		}
	}
}

