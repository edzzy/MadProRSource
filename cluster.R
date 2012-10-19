clusterAnalyse<-function(mat,comparaison,f,pvalRaw,info,pathPNG="./",pathAnnot="./",projet="",species="h",seuil=2){

	#Nom du fichier contenant toute les annotations
	filePuce<-paste(pathAnnot,"/",projet,"-puce.txt",sep="")

	#vecteur contenant le chemin de tous les graph
	graphFile<-c()

	if(!file.exists(filePuce)){
		puce<-info$GeneName
		write.table(puce,filePuce,sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
	}

	for(i in 1:ncol(comparaison)){

		selectCoord<-NULL
		#Detection
		pas=nrow(mat) *0.7/100
		coord<-detectCluster(pval=pval[,i],pas=pas,seuil=seuil)	
		#Extraction	
		prefixName<-paste(comparaison[1,i],comparaison[2,i],sep="-VS-")

		if(!is.null(coord)){
			extractCluster(coord,info,dir=pathAnnot,pref=prefixName)
			#Annotation
			fileList<-paste(pathAnnot,"/",prefixName,"/list.txt",sep="")
			resultDir<-paste(pathAnnot,"/",prefixName,"/resultat",sep="")

			if(file.exists(fileList)){
				commandAnnotation<-paste("gominer -p ",filePuce," -f ",fileList, " -s ", species, " -r ", resultDir,sep="")
				system(commandAnnotation)
#Evaluation
				clust<-1
				filesSfdr<-dir(path=resultDir, pattern="^S_*")
				gceName<-dir(path=resultDir,pattern="*gce*")
				Ename<-dir(path=resultDir, pattern="^E_*")
				GominerF=dir(path=resultDir, pattern="^[^E|S].*_fdrse")
			
				for (j in 1:length(filesSfdr)){
					if(nrow(read.delim(paste(resultDir,filesSfdr[j],sep="/"))) !=0){

						fileNamesGominer<-"rapport/AnnotationCluster.tex"
						if(!file.exists("rapport")){
							dir.create("rapport")
						}

						tmpFile<-paste(resultDir,"/",filesSfdr[j],sep="")

						title<-sub("S_(.*)_(\\d*)\\.txt.*fdrse.txt","\\1 : Cluster ",filesSfdr[j])
						title<-paste(title,clust,sep="")

						tmpFdrName<-sub("S_(.*)_(\\d*)\\.txt.*fdrse.txt","S_\\1",filesSfdr[j])
						tmpFdrName<-paste(tmpFdrName,"_",clust,".fdr.tsv",sep="")

						tmpGceName<-sub("(.*)_(\\d*)\\.txt.*gce.txt","\\1",gceName[j])
						tmpGceName<-paste(tmpGceName,"_",clust,".gce.tsv",sep="")

						EnameTmp<-sub("E_(.*)_(\\d*)\\.txt.*fdrse.txt","E_\\1",Ename[j])
						EnameTmp<-paste(EnameTmp,"_",clust,".fdr.tsv",sep="")

						gName<-paste(resultDir,GominerF,sep="/")
						file.remove(gName)


						#repertoire annotation question
						pathComp<-paste(pathAnnot,"/",prefixName,sep="")
						#Nom du fichier contenant la liste des gnènes
						prefix<-sub("S_(.*)_\\d*\\.txt.*fdrse.txt","\\1",filesSfdr[j])


						fileListCluster<-paste(pathComp,"/",prefix,"_",j,".txt",sep="")
						newFileListCluster<-paste(pathComp,"/",prefix,"_",clust,".txt",sep="")
						file.rename(fileListCluster,newFileListCluster)

						file.rename(paste(resultDir,filesSfdr[j],sep="/"),paste(resultDir,tmpFdrName,sep="/"))
						file.rename(paste(resultDir,gceName[j],sep="/"),paste(resultDir,tmpGceName,sep="/"))
						file.rename(paste(resultDir,Ename[j],sep="/"),paste(resultDir,EnameTmp,sep="/"))

						tex_tab2tex(paste(resultDir,tmpFdrName,sep="/"),fileNamesGominer,title=title)

						clust<-clust+1
						selectCoord<-rbind(selectCoord,coord[j,])


				}else{
					prefix<-sub("S_(.*)(_\\d*)\\.txt.*fdrse.txt","\\1\\2",filesSfdr[j])
					files2remove<-dir(path=resultDir,pattern=prefix)
					files2remove<-paste(resultDir,files2remove,sep="/")
					file.remove(files2remove)
				}
			}
		}
		#Graph cluster
	}
	out_name<-paste(pathPNG,"/",projet,prefixName,".png",sep="")
	graphFile<-c(graphFile,out_name)	

	f<-as.matrix(f)
	mat<-as.matrix(mat)
	m1<-meanByFact(mat,f,comparaison[1,i])
	m2<-meanByFact(mat,f,comparaison[2,i])
	logFC<-log2(m1/m2)
	value<-sign(logFC) * -log10(pval[,i])
#	value<-apply(tabmean,1,graphClustPval)
	if(is.null(coord)){
		selectCoord<-0	
	}
	
		graphMmobile(filename=out_name,value=value,seuil=seuil,clust=selectCoord)
	}

	return(graphFile)
}



detectCluster<-function(pval,seuil=2,seuilCluster=150,pas=200,out_name="detecClust.jpg" ){
	
	value<- -log10(pval)
	curveMobile<-rep(1/pas,pas)
	fil<-filter(value,curveMobile)
	select<- fil > seuil
	selectm1<-select[-1]
	selectm1[length(select)]<-NA
	
	#indice de début et de fin, +1 pour le début car le TRUE et sur le "brin" de longeur n-1

	begin <- which(select == FALSE & selectm1 == TRUE)
	end <- which(select == TRUE & selectm1 == FALSE)

	if(length(begin) != 0){
	
		#sens graphe est de longeur n-1 donne le signe de l'indice n+1 (indice 1 de sens graphe donne le sens de l indice 2 de value
		sensGraph<-sign(diff(fil))
		
		#Preparation à l'extension des pics : Recherche de la prochaine vallé pour le début et la fin
		newBegin<-sapply(begin,extendBegin,sensGraph)
		newEnd<-sapply(end,extendEnd,sensGraph)
	
		tailleCluster<-newEnd - newBegin
	
		selectBegin<-newBegin[which(tailleCluster >= seuilCluster)]
		selectEnd<-newEnd[which(tailleCluster >= seuilCluster)]
		coordClust<-fusionCluster(selectBegin,selectEnd)

		return(coordClust)

	}else{

		return(NULL)	
	}


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
extractCluster<-function(coordClust,info,pref="clust",dir="./"){

	fileList<-paste(dir,"/",pref,"/list.txt",sep="")
	for(i in 1:nrow(coordClust)){
		clust<-info[coordClust[i,1] : coordClust[i,2],]$GeneName
		pathName<-paste(dir,"/",pref,sep="")
		if(!file.exists(pathName)){
			dir.create(pathName)
			}
		outname<-paste(pathName,"/",pref,"_",i,".txt",sep="")
		cat(outname,"\n",file=fileList,append=TRUE,sep="")
		write.table(clust,outname,sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
	}
}
