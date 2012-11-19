`madPro` <-
function(pSetupFile="pSetup.txt",Normalise="L",clusteringALEA=TRUE,Filtrage=TRUE,Cluster=TRUE,tStat=TRUE,import=TRUE,Annotation=TRUE,clustering=TRUE,dCluster=TRUE){

if(import){
	nError = 0
	#-Importation du fichier de setup
	print("Debut Setup de l'analyse")
	print("Verification configuration analyse")
#################################################
	if(verifParam(pSetupFile)){
		message<-paste("Mauvaise configuration de ", pSetupFile,sep="")
		stop(message)
	}else{
		pSetup<-getParam(pSetupFile)	
	}

	projet<-getProjet(pSetup)
	typeArray<-getTypeArray(pSetup)
	dirFile<-getDirFile(pSetup)
	fileEch<-getFileEch(pSetup)
	normType<-getNormType(pSetup)
	filterParam<-getFilterParam(pSetup)
	nval<-getNval(pSetup)
	comparFile<-getComparFile(pSetup)
	comparaison<-read.delim(comparFile,header=FALSE)
	alea<-getAlea(pSetup)

############## PIPELINE ANALYSE###################
	logNames<-paste(projet,".log",sep="")
	cat("\n",date(),file=logNames,append=TRUE)
	cat("\nProjet : ",projet,append=TRUE)
	cat("\nSetup  OK",append=TRUE)
	rapportName<-paste(projet,"-rapport.tex",sep="")
	treepath<-create_pathway(projet)
	treepath<-as.data.frame(treepath)
	cpRapport(treepath)
	command<-paste("mv rapport/rapport.tex rapport/",rapportName,sep="")

	system(command)
	if(!file.exists(fileEch))
		stop("Le Fichier ",fileEch," est introuvable")
	###Annotation des echantillons########  
	print ("début annotation des echantillons")
	pData<-read.delim(fileEch,header=TRUE, row.names=1,sep="\t")
	frameFac<-rbind(t(pData)[-1:-2,])
	if(is.null(dim(frameFac))){
		frameFac<-as.data.frame(frameFac)
	}
	frameFacMA<-frameFac
	print ("fin annotation des echantillons")

	print("Debut annotation puce")
	namesFiles<-pData$nomFichiers


	# on lit la matrice
	if(typeArray == "GPR"){
  
		files<- dir(path=dirFile,pattern=".*\\.gpr$")
		files<-paste(dirFile,files,sep="/")
		dataMA<-read_GPR(files,namesArray)
	
	}else if(typeArray=="AG"){
  
		print("Puce de type Agilent")
		files<- dir(path=dirFile,pattern="U.*\\.txt$")
  
		if (!all(file.exists(paste(dirFile,pData$nomFichiers,sep="/"))))
			stop("Fichiers du fichier de config inexistant")

		files<-paste(dirFile,files,sep="/")
		infoGeneAnot<-getInfo(files[1])
		
		puceInfo<-getPuceInfo(getPattern(files[1]))
		print(puceInfo)
		species<-as.character(puceInfo$Species)
		dye<-as.character(puceInfo$dye)

		dataMA<-read_Agilent(pData=pData,namesGenes=as.character(row.names(infoGeneAnot)),type="AG",pathDir=dirFile)

		fileName<- paste(projet,"-rawdataCtrlInfo.txt",sep="")
		fileName<-paste(dirFile,fileName,sep="/")
		tmpinfoGene<-infoGeneAnot[rownames(dataMA),]
		tmpDataMA<-cbind(tmpinfoGene,dataMA)
		if(!file.exists(fileName)){
			write.table(tmpDataMA,fileName,row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)
		}
		fileName<- paste(projet,"-rawdataCtrlInfo.txt",sep="")
	
		fileName<-paste(treepath$resultatMat,fileName,sep="")
		if(!file.exists(fileName)){
			write.table(tmpDataMA,fileName,row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)
		}
		rm(tmpDataMA)

	
		control<-which(infoGeneAnot$ControlType != 0)
		dataMA<-dataMA[-control,]

		fileName<- paste(projet,"-rawdata.txt",sep="")
		fileName<-paste(dirFile,fileName,sep="/")
		if(!file.exists(fileName)){
			write.table(dataMA,fileName,col.names=NA,sep="\t",quote=FALSE);
		}

	}else if(typeArray=="NG"){
       
	files<- dir(path=dirFile,pattern=".*\\.pair$")
  
	if (all(files != namesFiles))
		stop("Non correspondance entre les noms du fichiers d'annotation et les noms reels")
		files<-paste(dirFile,files,sep="/") 
		read_NG(namesEch,files)
	}

	tex_importData(typeArray,dye)
	save(projet,frameFac,dataMA,pData,pSetup,puceInfo,treepath,comparaison,infoGeneAnot,file=paste(projet,"-dataRaw.Rdata",sep=""))
}


if(Normalise == "L" | Normalise =="Q"){
		

	fd<-paste(projet,"-01-stat-descriptive/",sep="")

	#######GRAPHIQUES AVANT NORMALISATION####
	##creation du boxplot
	print("Raw BoxPlot")
	
	output_name<-paste(treepath$stat,projet,"-raw-boxplot.jpeg",sep="")
	if(!file.exists(output_name)){
		create_boxplot(dataMA,output_name)
	}
	##Correlation
	print("Correlation")
	cat("** \nCorrelation avant normalisation\n",append=TRUE,file=logNames)
	output_name = paste(treepath$stat,projet,"-raw-correlation.jpeg",sep="")
	if(!file.exists(output_name)){
		echBadCor<-create_correlation_median(dataMA,output_name)

		if(!is.null(echBadCor)){
			cat("echantillons dont la correlation avec le profil median est <0.8",echBadCor,"\n",append=TRUE,file=logNames)
		}else{
			cat("OK\n",append=TRUE,file=logNames)
		}
	}
	####NORMALISATION  (LOWESS)##########

	print("Normalisation")
	#fd<-paste(treepath$normalisation,"/",sep="")
	nomFile = paste(treepath$normalisation,projet, "-normalisation.txt", sep="")
	if(!file.exists(nomFile)){
		if(Normalise == "L"){
			dataN<-LOWESS(nom_fichier="",data=dataMA,pngDir=treepath$normalisation,profil.median="NA",graph=1,projet=projet)
		}else if(Normalise =="Q"){
			dataN<-normQuantile(mat=as.matrix(dataMA),pngDir = treepath$normalisation)
		}

		write.table(dataN,nomFile,sep="\t",row.names=TRUE, col.names=NA,quote=FALSE)
		fileName<- paste(treepath$normalisation,projet,"-normalisationInfo.txt",sep="")
		tmpinfoGene<-infoGeneAnot[rownames(dataN),]
		tmpDataMA<-cbind(tmpinfoGene,dataN)
		write.table(tmpDataMA,fileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE);
		fileName<- paste(treepath$resultatMat,projet,"-normalisationInfo.txt",sep="")
		write.table(tmpDataMA,fileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE);
		rm(tmpDataMA)
	
		fd<-paste(fd,"images/",sep="")
		imgNames<-paste(treepath$normalisationImage,projet,"-norImg.png",sep="")

	#images comprenant tous les plot de normalisation 4 échantillons par image (avant apres avant/apres)
		print("montage image")
		montage_cmd<-paste(" montage ",treepath$normalisationImage,"*.png -tile 3x4 -geometry 100% -background none ", imgNames,sep="")
		print(system.time(try(system(montage_cmd))))

		imgMont<-dir(path=as.character(treepath$normalisationImage),pattern="*-norImg*")
		filesNorm<-paste(treepath$normalisationImage,imgMont,sep="")
	
#	dataN = read.delim(paste(treepath$normalisation,projet,"-",nom_fichier,"-normalisation.txt",sep=""), sep="\t", header=TRUE, comment.char="",row.names=1)
		tex_norm(projet,filesNorm)
		print("Fin Normalisation")
	
	}	
	###GRAPHIQUE APRES NORMALISnormalisatioaATION#####
	##creation du boxplot
	print("Boxplot Norm")
	output_name = paste(treepath$normalisationImage,projet,"-lowess-boxplot.jpeg",sep="")
	if(!file.exists(output_name)){
		create_boxplot(dataN,output_name)
	}	
	#create_stats_des(dataN,output_name,minMax=FALSE)
	##Correlation apres normalisation
	print("Correlation Norm")
	cat("** \nCorrelation apres normalisation\n",append=TRUE,file=logNames)
	output_name = paste(treepath$normalisationImage,projet,"-lowess-correlation.jpeg",sep="")
	if(!file.exists(output_name)){
		echBadCorNorm<-create_correlation_median(dataN,output_name)
	
		if(!is.null(echBadCorNorm)){
			cat("echantillons dont la correlation avec le profil median est <0.8",echBadCorNorm,"\n",append=TRUE,file=logNames)
		}else{
			cat("OK\n",append=TRUE,file=logNames)
		}
	}
		print("ecriture rapport preproc")
		##Génération fichiers TEX stat Descr et corrélation
		tex_boxplot(projet)
		tex_Corre(projet,echBadCor,echBadCorNorm)

		save(projet,frameFac,dataN,pData,pSetup,puceInfo,treepath,infoGeneAnot,comparaison,file=paste(projet,"-dataNorm.Rdata",sep=""))
	
}

if(Filtrage==TRUE){
	if(is.null(dim(frameFac))){
		annotFilter<-as.factor(as.character(unlist(frameFac)))
	}else{
		annotFilter<-as.factor(as.character(unlist(frameFac[filterParam,])))
	}
	nbclasses <- nlevels(annotFilter)
	##############################
	#creation d'une matrice reduite
	if (clusteringALEA == TRUE){

		print("matrice aleatoire")
		nProbes<-nrow(dataN)*(alea/100)
		sampMatrix<-sampleMatrix(dataN,nProbes)
		sampleMPrefix=paste(projet,"-03-clusterAleatoire/",projet,alea,"sample",sep="")
		sampleMPrefixShort = paste(projet,alea,"sample",sep="")
		sampleMName<-paste(sampleMPrefix,".txt",sep="")
		write.table(sampMatrix,sampleMName,quote=FALSE,sep="\t",col.names=NA,row.names=TRUE)

		################
		###Cluster######
		sampMatrix<-clusterEinsen(f=sampleMName)
	#	print("debut clustering matrice aleatoire")
	#	commandCluster<-paste(" cluster -f ",sampleMName," -l  -cg m -g 1 -e 1  -m c",sep="")
	#	print(system.time(try(system(commandCluster,intern=TRUE))))
	#	print("fin clustering matrice aleatoire")
	#	print("image matrice cluster aleatoire")
		commandSlcviewMatrix<-paste(" slcview.pl ",sampleMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -gtrresolution 0 -arraylabels 0 -atrresolution 0 -o ",sampleMPrefix,"Matrix.png" ,sep="" )
	
		print(system.time(try(system(commandSlcviewMatrix))))

	
		commandSlcviewArray<-paste(" slcview.pl ",sampleMPrefix,".cdt -xsize 25 -height 1 -gtrresolution 0 -genelabel 0 -noimage -o ",sampleMPrefix,"Array.png" ,sep="" )
	
		print(system.time(try(system(commandSlcviewArray))))
	
	
	
	################
	###Filtrage######
#		nameCDT<-paste(projet,"-03-clusterAleatoire/",projet,"-",nom_fichier,alea,"sample.cdt",sep="")
#		print("filtrage matrice aleatoire")
#		matSamplecdt<-read.delim(nameCDT,sep="\t")
#
#	#ordonne la matrice normalisé
#		nG<-as.vector(matSamplecdt[,2])
#		nG<-nG[-1:-2]
#		nG<-as.character(nG)
#		nA<-colnames(matSamplecdt)
#		nA<-nA[-1:-4]
#		rm(matSamplecdt)
#	###############
#		sampMatrix<-sampMatrix[,nA]
#		sampMatrix<-sampMatrix[nG,]
	#####################Utilisation de matrix2png pour le clustering de la matrice aleatoire
	
		mapName<-paste(projet,"-color.txt",sep="")
		frameFac<-as.data.frame(frameFac)[,colnames(sampMatrix)]
		frameFacN<-frameFac
		frameNames<-paste(projet,"-03-clusterAleatoire/",projet,"-frameFac.txt",sep="")
		write.table(frameFac,frameNames,sep="\t",row.names=FALSE,quote=FALSE);
		outfileColor<-paste(projet,"-03-clusterAleatoire/",projet,"-colorSample.png",sep="")
		colorName<-paste("makeColor.pl -c ", mapName, " -p " , frameNames ,"  -o ", outfileColor,sep="")
		system(colorName)
	
	

		##############################################
		
		
		
		#choix du seuil de filtrage
		med.sample<-apply(sampMatrix,2,median)
		seuil=mean(med.sample)
		#annotation du parametre choisi pour le filtrage dans le sens du clustering
		#Si il n'y a qu'un parametre il faut tester (conflit data.frame et vecteur)
		#filtre<-result_filter(filtrage_non_exprimes(sampMatrix,nbclasses,annotFilter,seuil,nval)
		#filtre<-as.numeric(result_filter)
		result_filter<-filtrage_non_exprimes(sampMatrix,nbclasses,annotFilter,seuil,nval)
		filtre<-as.numeric(result_filter)
		########Graph visualisation du filtre choix des sondes
		filename=paste(projet,"-03-clusterAleatoire/",projet,"-filtre.png",sep="")
		graphMmobile(filename,filtre)
		######Visualisation du signal median et du seuil
		profmed<-apply(sampMatrix,1,median)
		profmed<-log10(profmed)
		lseuil<-log10(seuil)
		filename=paste(projet,"-03-clusterAleatoire/",projet,"-signalMedian.png",sep="")
		graphMmobile(filename,profmed,lseuil)
		#########################
		montageMont<-paste("madProMontage.pl -b ",outfileColor," -m ",projet,"-03-clusterAleatoire/",sampleMPrefixShort,"Matrix.png -t ",projet,"-03-clusterAleatoire/atr.",sampleMPrefixShort,"Array.png -g ",filename," -o ",projet,"-03-clusterAleatoire/",projet,"-filtrageCluster.png",sep="")
		system(montageMont)
		montageTree<-paste("montage ",projet,"-03-clusterAleatoire/atr.",sampleMPrefixShort,"Array.png ",outfileColor," -geometry +0+0 -tile 1x ",projet,"-03-clusterAleatoire/",projet,"-TreeArraycolor.png",sep="")
		system(montageTree)
		fileTree=paste(projet,"-03-clusterAleatoire/",projet,"-TreeArraycolor.png",sep="")
		rotateTree<-paste("convert ",fileTree, " -rotate 90 ",fileTree,sep="")
		system(rotateTree)
	
	}

	
	####Filtrage matrice totale
	print("filtrage matrice totale")
	if(is.null(colnames(frameFac))){
		dataN<-dataN[,match(colnames(dataN),names(frameFac))]
	}else{
		dataN<-dataN[,match(colnames(dataN),colnames(frameFac))]
	}
	seuil<-mean(apply(dataN,2,median))
	print(dim(dataN))
	print(nbclasses)
	print(annotFilter)
	print(seuil)
	print(getNval(pSetup))
	result_filter=filtrage_non_exprimes(dataN,nbclasses,annotFilter,seuil,nval)
	
	
	m.filtered=dataN[result_filter == TRUE,]
	
	tex_filtrage(projet,annotFilter,seuil,nrow(dataN),nrow(m.filtered),nval)
	
	
	cat("\nseuil",seuil,file=logNames,sep="\t",append=TRUE)
	cat("\nnombre de sonde total",nrow(dataN),file=logNames,sep="\t",append=TRUE)
	cat("\nnombre de sonde filtree",nrow(m.filtered),file=logNames,sep="\t",append=TRUE)
	
	
	
	#filterName<-paste(projet,"-04-filtre/",projet,"-matrix_filtree.txt",sep="")
	if(clustering == FALSE){
		filterMPrefix<-paste(treepath$filtre,projet,"-matrix-filtree",sep="")
		filterName<-paste(filterMPrefix,".txt",sep="")
		write.table(m.filtered,filterName,col.names=NA,row.names=TRUE,sep="\t",quote=FALSE)
		filterMPrefix<-paste(treepath$resultat,projet,"-matrix-filtree",sep="")
		filterName<-paste(filterMPrefix,".txt",sep="")
		write.table(m.filtered,filterName,col.names=NA,row.names=TRUE,sep="\t",quote=FALSE)
		save(projet,frameFac,m.filtered,pData,pSetup,puceInfo,treepath,comparaison,infoGeneAnot,file=paste(projet,"-dataNorm.Rdata",sep=""))
	}
}	
if(clustering==TRUE){	
	filterMPrefix<-paste(treepath$filtre,projet,"-matrix-filtree",sep="")
	filterName<-paste(filterMPrefix,".txt",sep="")
	################Clustering de la matrice totale
	print("debut clustering matrice totale")
	write.table(m.filtered,filterName,col.names=NA,row.names=TRUE,sep="\t",quote=FALSE)
	m.filtered<-clusterEinsen(filterName)
#	commandCluster<-paste(" cluster -f ",filterName," -l  -cg m -g 1 -e 1  -m c",sep="")
#	print(system.time(system(commandCluster)))
		
	print("fin clustering matrice totale")
	frameFac<-frameFac[,colnames(m.filtered)]
	commandSlcviewMatrix<-paste("slcview.pl ",filterMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -gtrresolution 0 -arraylabels 0 -atrresolution 0 -o ",filterMPrefix,"Matrix.png" ,sep="" )
		
	print(system.time(system(commandSlcviewMatrix)))
		
	commandSlcviewArray<-paste("slcview.pl ",filterMPrefix,".cdt -xsize 25 -height 1 -gtrresolution 0 -genelabel 0 -noimage -o ",filterMPrefix,"Array.png" ,sep="" )
		
	print(system.time(system(commandSlcviewArray)))
	
		
	
	
	filterNamecdt<-paste(projet,"-04-filtre/",projet,"-matrix-filtree.cdt",sep="")
#	matcdt<-read.delim(filterNamecdt,sep="\t")
#	#ordonne la matrice normalisé filtre selon le cdt
#	nG<-as.vector(matcdt[,2])
#	nG<-nG[-1:-2]
#	nG<-as.character(nG)
#	m.filtered<-m.filtered[nG,]
#	frameFacF<-frameFac
	infocdt<-infoGeneAnot[as.character(row.names(m.filtered)),]
	newcdt<-cbind(infocdt,m.filtered)
#	colnames(newcdt)[1]<-"GID"
	write.table(newcdt,filterNamecdt,sep="\t",row.names=TRUE,quote=FALSE)
	 rm(newcdt)
	 rm(matcdt)
	
	########Utilisation de matrix2png pour visualation des parametres
	frameNames<-paste(projet,"-04-filtre/",projet,"-frameFacF.txt",sep="")
	write.table(frameFac,frameNames,sep="\t",row.names=FALSE,quote=FALSE);
	outfileColor<-paste(projet,"-04-filtre/",projet,"-colorSample.png",sep="")
	colorName<-paste("makeColor.pl -c ", mapName, " -p " , frameNames ,"  -o ", outfileColor,sep="")
	system(colorName)
	
	montageTree<-paste("montage ",projet,"-04-filtre/atr.",projet,"-matrix-filtreeArray.png ",outfileColor," -geometry +0+0 -tile 1x ",projet,"-04-filtre/",projet,"-TreeArraycolor.png",sep="")
	system(montageTree)
	fileTree=paste(projet,"-04-filtre/",projet,"-TreeArraycolor.png",sep="")
	rotateTree<-paste("convert ",fileTree, " -rotate 90 ",fileTree,sep="")
	system(rotateTree)
	
	
		filterName<-paste(projet,"-04-filtre/",projet,"-matrix_filtreeInfo.txt",sep="")
		tmpinfoGene<-infoGeneAnot[rownames(m.filtered),]
		tmpDataMA<-cbind(tmpinfoGene,m.filtered)
		write.table(tmpDataMA,filterName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE);
		rm(tmpDataMA)
	
		save(projet,frameFac,m.filtered,pData,pSetup,puceInfo,treepath,comparaison,infoGeneAnot,file=paste(projet,"-dataFilter.Rdata",sep=""))


}
########test stat
if(tStat==TRUE){
	if(clustering == FALSE){
		load(dir(pattern="*-dataFilter.Rdata"))
	}
	print("test stat")
	finalPV <- NULL
	finalFC <- NULL
	seuilPval <- 0.05
	seuilPvalRaw <- 0.001
	path<-paste(projet,"-05-student",sep="")
	pvalRaw<-data.frame()
	if(Annotation && !is.null(infoGeneAnot$GeneName)){
		pathAnot<-paste(projet,"-06-annotation",sep="")
		filePuce<-paste(pathAnot,"/",projet,"-puce.txt",sep="")
		puce<-infoGeneAnot[rownames(m.filtered),]$GeneName
		write.table(puce,filePuce,sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
		
	}
	
	 
	  if(is.null(comparaison)){
		  if(is.null(colnames(frameFac))){
			  comparaison<-combn(levels(as.factor(unlist(frameFac))),2,simplify=TRUE)
		  }else{
			  comparaison<-combn(levels(as.factor(unlist(frameFac[filterParam,]))),2,simplify=TRUE)
		  }
	  }
	  #-calcul des Test STAT
	
	  path<-paste(projet,"-05-student",sep="")
	
	  tabGenDiff<-matrix(data=NA,ncol=4)
	  nb<-paste("NB pvalAdjust < ",seuilPval,sep="")
	  colnames(tabGenDiff)<-c("versus",nb,"Up","Down")
	
	
	  tabGenDiffRaw<-matrix(data=NA,ncol=4)
	  nbRaw<-paste("NB pvalRaw < ",seuilPvalRaw,sep="")
	  colnames(tabGenDiffRaw)<-c("versus",nb,"Up","Down")
	
	  fileTexCluster<-NULL
	  nbListGene = 0 # nombre de liste de gene diff à annoter
	
	
	  for(i in 1:ncol(comparaison)){
		  #- test stat
	  		
		  	versus<-paste(comparaison[1,i],"-Vs-",comparaison[2,i],sep="")
		  	pathDir<-paste(path,"/Comparaison-",versus,sep="")
		  	if(!file.exists(pathDir))
		  		dir.create(pathDir)
	
			dataPval<-pairRows.t.test(comparaison[,i],m.filtered,frameFac,padj.method="BH",path=path,graph=TRUE,projet=projet)
			colnames(dataPval)<-c(paste(versus,"Raw",sep=""),paste(versus,"Adjust",sep=""))
			result_pvalAdjust<-dataPval[,2] #Pvalue ajustées
			result_pvalRaw<-dataPval[,1] #Pvalue brutes 
			if(i == 1){
				pvalRaw<-cbind(result_pvalRaw)
			}else{
				pvalRaw<-cbind(pvalRaw,result_pvalRaw)
	
			}
	
		  finalPV<-cbind(finalPV,dataPval)
		  # selection des genes differentiels			
	
		  pvalSelect<- subset(result_pvalAdjust,result_pvalAdjust <= seuilPval)
		  pvalSelectRaw<- subset(result_pvalRaw,result_pvalRaw <= seuilPvalRaw)
		  
		  
		  r<-c(versus,length(pvalSelect))	
		  if(!is.null(pvalSelect))
			write.table(pvalSelect,file=paste(pathDir,"/",projet,"-",versus,"-",seuilPval,"Adjust.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	
		rRaw<-c(versus,length(pvalSelectRaw))	
	
		  if(!is.null(pvalSelectRaw))
			write.table(pvalSelectRaw,file=paste(pathDir,"/",projet,"-",versus,"-",seuilPvalRaw,"-Raw.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	
	
	
		  #-Fold Change 
		  m1<-meanByFact(m.filtered,frameFac,comparaison[1,i])
		  m2<-meanByFact(m.filtered,frameFac,comparaison[2,i])
		  #matMean<-cbind(m1,m2)
		  fc<-log2(m1/m2)
		  #fc<-apply(matMean,1,FC)
			
		  finalFC<-cbind(finalFC,fc)
		  colnames(finalFC)[i]<-paste("FC-",versus,sep="")
	
		  #- gene up ou down Adjust
		  syntheseStat<-cbind(result_pvalAdjust,fc)
		  up<-syntheseStat[which(result_pvalAdjust<= seuilPval & fc >= 0),]
		  down<-syntheseStat[which(result_pvalAdjust<= seuilPval & fc < 0),]
		  nup<-nrow(up)
		  if(is.null(nup))
			  nup<-0
		  ndown<-nrow(down)
		  if(is.null(ndown))
			  ndown<-0
	
		  r<-c(r,nup,ndown)
		  if(is.na(tabGenDiff[1,1])){
		  	tabGenDiff[1,]<-r
		  }else{
		  	#n<-nrow+1
		  	tabGenDiff<-rbind(r,tabGenDiff,deparse.level=0)
		  }
		  colnames(syntheseStat)[1]<-paste("Pvalue-",versus,sep="")
		  colnames(syntheseStat)[2]<-paste("FoldChange-",versus,sep="")
	
	
		  #- gene up ou down Adjust
		  syntheseStatRaw<-cbind(result_pvalRaw,fc)
		  upRaw<-syntheseStat[which(result_pvalRaw<= seuilPvalRaw & fc >= 0),]
		  downRaw<-syntheseStat[which(result_pvalRaw<= seuilPvalRaw & fc < 0),]
		  nupRaw<-nrow(upRaw)
		  if(is.null(nupRaw))
			  nupRaw<-0
		  ndownRaw<-nrow(downRaw)
		  if(is.null(ndownRaw))
			  ndownRaw<-0
	
		  rRaw<-c(rRaw,nupRaw,ndownRaw)
		  if(is.na(tabGenDiffRaw[1,1])){
		  	tabGenDiffRaw[1,]<-rRaw
		  }else{
		  	#n<-nrow+1
		  	tabGenDiffRaw<-rbind(rRaw,tabGenDiffRaw,deparse.level=0)
		  }
		  colnames(syntheseStatRaw)[1]<-paste("Pvalue-",versus,sep="")
		  colnames(syntheseStatRaw)[2]<-paste("FoldChange-",versus,sep="")
	
		  #-fichier de synthese
		 if(nup != 0)
			write.table(up,file=paste(pathDir,"/",projet,"-",versus,"-upAdjust.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
		 if(ndown !=0)
			write.table(down,file=paste(pathDir,"/",projet,"-",versus,"-downAdjust.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	
	
		#Fichiers pour l'annotation de toutes les listes
		if(Annotation && !is.null(infoGeneAnot$GeneName)){
	
			GeneName<-infoGeneAnot$GeneName
			names(GeneName)<-rownames(infoGeneAnot)
			fileList<-paste(pathAnot,"/",projet,"-listeFile.txt",sep="")
	
			if(nup != 0){
				upAnotFile<-paste(pathAnot,"/",projet,"-",versus,"-UP.txt",sep="")
				cat(upAnotFile,"\n",file=fileList,append=TRUE,sep="")
				nbListGene<- nbListGene + 1
				upAnot<-GeneName[rownames(up)]
				write.table(upAnot,file=upAnotFile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep="\t")
	
			}
			if(nup != 0){
				downAnotFile<-paste(pathAnot,"/",projet,"-",versus,"-DOWN.txt",sep="")
				cat(downAnotFile,"\n",file=fileList,append=TRUE,sep="")
				nbListGene<- nbListGene + 1
				downAnot<-GeneName[rownames(down)]
				write.table(downAnot,file=downAnotFile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep="\t")
			}		
	
		}
	
	
		  write.table(syntheseStat,file=paste(pathDir,"/",projet,"-",versus,"-allSynth.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
					
	  }
	
		save(projet,frameFac,m.filtered,pData,pSetup,puceInfo,treepath,pvalRaw,comparaison,infoGeneAnot,file=paste(projet,"-dataFilter.Rdata",sep=""))
}
if(dCluster==TRUE){
	seuilPic =2 
#detection des cluster
	if(tStat == FALSE){
		load(dir(pattern="*-dataFilter.Rdata"))
		species<-puceInfo$Species
		outfileColor<-paste(projet,"-03-clusterAleatoire/",projet,"-colorSample.png",sep="")
	}
	species<-puceInfo$Species
	ylim<-round(max(abs(-log10(pvalRaw))),0)
	ylim<-ylim+1
	pathPNG<-paste(projet,"-05-student",sep="")
	pathAnnot<-paste(projet,"-06-annotation",sep="")
	print(projet)
	print(dim(pvalRaw))	
		graphFile<-clusterAnalyse(mat=m.filtered,info=infoGeneAnot,comparaison=comparaison,f=frameFac,pvalRaw=pvalRaw,pathPNG=pathPNG,pathAnnot=pathAnnot,projet=projet,species=species,seuil=seuilPic,ylim=ylim)
		print(graphFile)

	  	fileMatrix <-paste(projet,"-04-filtre/",projet,"-matrix-filtreeMatrix.png",sep="")
	  	fileTree <- paste(projet,"-04-filtre/atr.",projet,"-matrix-filtreeArray.png",sep="")
	  fileTexCluster<-NULL

		for(i in 1:length(graphFile)){

			outImage<-sub("(.*).png","\\1",graphFile[i])
			outImage<-paste(outImage,"-Stat.png",sep="")

			graphStatClust(matrixImage=fileMatrix,boxImage=outfileColor, treeImage=fileTree,statImage=graphFile[i],outImage=outImage)	
			versus<-paste(comparaison[1,i],"-VS-",comparaison[2,i],sep="")
			if(i == 1){
				appendFirst = FALSE
			}else{
				appendFirst = TRUE
			}

			tex_clusterImage(outImage, paste("rapport/graphCluster.tex",sep=""),versus,appendFirst,projet)
			if(is.null(fileTexCluster)){
				fileTexCluster<-paste(versus,".tex",sep="")
			}else{
				fileTexCluster<-c(fileTexCluster,paste(versus,".tex",sep=""))
			}
		}


}
if(Annotation==TRUE){
#	tex_question(fileTexCluster,paste("rapport/graphCluster.tex",sep=""))
	write.table(finalPV,file=paste(path,"/",projet,"-allpval.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	write.table(finalFC,file=paste(path,"/",projet,"-allFC.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	print(tabGenDiff)
	tex_genDiff(tabGenDiff)
	tex_genDiff(tabGenDiffRaw,fileName="rapport/genDiffRaw.tex")
	if(Annotation && nbListGene != 0){
		resultDir<-paste(pathAnot,"/resultat",sep="")
		commandAnnotation<-paste("gominer -p ",filePuce," -f ",fileList, " -s ", species, " -r ", resultDir,sep="")
		system(commandAnnotation)
		filesGominer<-dir(path=resultDir, pattern="^S_*")
		print(filesGominer)
		for (i in 1:length(filesGominer)){
			fileNamesGominer<-"rapport/annotGeneDiff.tex"
			tmpFiles<-paste(resultDir,"/",filesGominer[i],sep="")
			title<-sub("S_(.*)-(UP|DOWN).*","\\1 : \\2",filesGominer[i])
			tex_tab2tex(tmpFiles,fileNamesGominer,title=title)
		}

		
	}else{
		print("Aucun gene diff pour aucune question")
	}

	info<-toLatex(sessionInfo(),local=FALSE)
	write(info,"rapport/info.tex")
	print("FIN")


	  
}


}
