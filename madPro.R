`madPro` <-
function(pSetupFile="pSetup.txt",savingData=TRUE,importData=TRUE,rmCTRL=TRUE,isCompar=FALSE,Normalise=TRUE,statBeforNorm=TRUE,statBeforeNorm=TRUE,clusteringALEA=TRUE,Filtrage=TRUE,Cluster=TRUE,tStat=TRUE,rmArray=FALSE,Annotation=FALSE,testUnit=FALSE){

#Package a charger pour l'excussion du script
require("limma")
require("genefilter")
#test Branche

nError = 0
CheckError=FALSE
#-Importation du fichier de setup
print("Debut Setup de l'analyse")
if(!file.exists(pSetupFile))
  stop("Le fichier", pSetupFile," n'est pas present")
pSetup<-read.delim(pSetupFile,row.names=1,header=FALSE,stringsAsFactors=FALSE,sep="\t")
pSetup<-t(pSetup)
pSetup<-as.data.frame(pSetup)
if(nrow(pSetup) != 1)
	stop("Mauvais formatage du fichier ",pSetupFile,". Nombre de colone > 1. Veuillez vérifier si il n'existe pas de tabulations parasite en fin de lignes.") 
cat("\n",date(),"\n",file="error.log",append=TRUE)

#-Import/Verification du nom de projet

if(!is.null(pSetup$Projet)){
	projet<-as.character(pSetup$Projet)
}else{
	CheckError=TRUE
	cat("Le nom du projet est manquant dans le fichier : ", pSetupFile,"\n",file="error.log",append=TRUE)
	nError <- nError +1
}

#-Import/Verification du prefixe des noms des matrices. 

if(!is.null(pSetup$nom)){
	nom_fichier <- as.character(pSetup$nom)
}else{
	CheckError=TRUE
	cat("Le prefixe du projet est manquant dans le fichier : ", pSetup,"\n",file="error.log",append=TRUE)
	nError <- nError +1
}

#Import/Verification du type d'array agilent=AG ou gpr=GPR nimbelgen=NG
if(!is.null(pSetup$typeArray)){
	typeArray<- as.character(pSetup$typeArray) 
	typeArray<-toupper(typeArray)
	if(typeArray != "AG" & typeArray != "GPR" & typeArray != "NG"){
		CheckError=TRUE
		cat("Le type d'array n'est pas le bon dans le fichier : ", pSetupFile," indiquer AG pour agilent, NG pour nimbelGen ou GPR pour genepix\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
	CheckError=TRUE
	cat("Le type d'array n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
	nError <- nError +1
}

#nombre de sondes pour la test matrice aleatoire.

if(!is.null(pSetup$alea)){
	alea<-as.numeric(as.character(pSetup$alea))
	if(alea < 0 | alea > 100){
		CheckError=TRUE
		cat("Le parametre alea doit être compris entre 0 et 100 il s'agit d'un pourcentage.\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
		CheckError=TRUE
		cat("Le parametre alea n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
}

#Fichier annotation des echantillons

if(!is.null(pSetup$fileEch)){
	fileEch<-as.character(pSetup$fileEch)
	if(!file.exists(fileEch)){
		CheckError=TRUE
		cat("Le fichier d'annotation des echantillons ",fileEch," est introuvable.\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
		CheckError=TRUE
		cat("Le parametre fileEch n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1

}
###

if(!is.null(pSetup$fileGene)){
	fileGene<-as.character(pSetup$fileGene)
	if(!file.exists(fileGene)){
		CheckError=TRUE
		cat("Le fichier des madProID ",fileGene," est introuvable.\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
		CheckError=TRUE
		cat("Le parametre fileGene n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
}

####
if(!is.null(pSetup$dirFile)){
	dirFile<-as.character(pSetup$dirFile)
	if(!file.exists(dirFile)){
		CheckError=TRUE
		cat("Le repertoire : ",dirFile," est introuvable.\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
		CheckError=TRUE
		cat("Le parametre dirFile n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
}

###
if(!is.null(pSetup$nval)){
	nval<-as.numeric(as.character(pSetup$nval))
	if(nval < 0 | nval > 100){
		CheckError=TRUE
		cat("Le parametre nval doit être compris entre 0 et 100 il s'agit d'un pourcentage.\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
		CheckError=TRUE
		cat("Le parametre nval n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
}
#nbclasses<-as.numeric(pSetup$nclasses)

if(!is.null(pSetup$filterParam)){
	filterParam<-as.numeric(as.character(pSetup$filterParam))
}else{
		CheckError=TRUE
		cat("Le parametre filterParam n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
}

if(!is.null(pSetup$dye)){
	dye<-as.numeric(as.character(pSetup$dye))
	if(dye != 1 & dye !=2 ){
		CheckError=TRUE
		cat("Le parametre dye doit être 1 ou 2.\n", file="error.log",append=TRUE)
		nError <- nError +1
	}
}else{
		CheckError=TRUE
		cat("Le parametre dye n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
}
##-Test parametre si le fichier contenant les controles est présents.

if(rmCTRL == TRUE){
	if(!is.null(pSetup$controle)){
		controle<-as.character(pSetup$controle)
		if(!file.exists(controle)){
			cat("Le fichier : ",controle , " contenant les controle est introuvable\n",file="error.log",append=TRUE)
		nError <- nError +1
			CheckError=TRUE
		}
	}else{
		cat("Le parametre controle n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
		CheckError=TRUE
	}
}

geneAnnot = FALSE
if(!is.null(pSetup$info)){
	infoGene<-as.character(pSetup$info)
	if(!file.exists(infoGene))
  	stop("Le fichier ",infoGene, " est introuvable")
	infoGeneAnot<-read.delim(infoGene,header=TRUE,sep="\t",row.names=1)
	geneAnnot = TRUE
}

if(is.null(pSetup$comparFile)){
	comparaison<-NULL
}else{
	comparFile<-as.character(pSetup$comparFile) 
	if(!file.exists(comparFile))
		stop("le fichier", comparFile, " est introuvable") 
		comparaison<-read.delim(comparFile,header=FALSE, sep="\t")
}

if(is.null(pSetup$ratio)){
  ratio<-NULL
}else{
	ratio<-as.character(pSetup$ratio) 
  ratio<-unlist(strsplit(ratio,"/"))
}

if(is.null(pSetup$bicoul)){
  bicoul<-FALSE
}else{
  bicoul<-as.character(pSetup$bicoul)
  if(!(bicoul == "TRUE" | bicoul == "FALSE")){
    cat("Le parametre bicoul doit être boolean TRUE ou FALSE\n",file="error.log",append=TRUE)
  	nError <- nError +1
			CheckError=TRUE
    }else{
      if(bicoul == "TRUE"){
        bicoul<-TRUE
      }else{
        bicoul<-FALSE
      }
      
      
    }
}
if(rmArray == TRUE){
	if(!is.null(pSetup$rmArrayFile)){
		rmArrayFile<-as.character(pSetup$rmArrayFile)
		if(!file.exists(rmArrayFile))
			stop("le fichier", rmArrayFile, " est introuvable") 
			arrayRm<-read.delim(rmArrayFile,header=FALSE, sep="\t",stringsAsFactor = FALSE)
			arrayRm<-unlist(arrayRm)
	}else{
		cat("Le parametre rmArrayFile n'est pas spécifié dans le fichier : ", pSetupFile,"\n", file="error.log",append=TRUE)
		nError <- nError +1
		CheckError=TRUE
	}
}
if(!is.null(pSetup$species)){
	species = as.character(pSetup$species)
	if(species == "h" || species == "m" || species == "r" || species == "d"){
		Annotation=TRUE
	}else{
		stop("Espece pour l'annotation incorecte h : Human, m : Mouse : r : Rat , d : Chien")
	}
	
}
if(CheckError){
	stop("Il y a eu ",nError," erreurs lors du setup du pipeline veillez regarder le fichier error.log pour les corriger" )
}
print ("Fin Setup de l'analyse")
#################################################





############## PIPELINE ANALYSE###################
logNames<-paste(projet,".log",sep="")
cat("\n",date(),file=logNames,append=TRUE)
cat("\nProjet : ",projet,append=TRUE)
cat("\nSetup  OK",append=TRUE)
if(!file.exists(fileEch))
  stop("Le Fichier ",fileEch," est introuvable")
###Annotation des echantillons########  
print ("début annotation des echantillons")
frameSample<-read.delim(fileEch)
namesArray<-createNamesArray(frameSample,type=typeArray,dye=dye,ratio)
frameFac<-createFactor4matrix2png(frameSample,namesArray,dye=dye,ratio)
if(is.null(dim(frameFac))){
	frameFac<-as.data.frame(frameFac)
}
frameFacMA<-frameFac
print ("fin annotation des echantillons")
 ##############################################


namesFiles<-frameSample$nomFichiers

if(fileGene != ""){
  test<- dir(pattern=fileGene)
  answer<-paste("le fichier :",fileGene,"n'est pas present",sep=" ")
  if(!file.exists(fileGene))
    stop("Le fichier : ", fileGene," n'est pas present", sep="")
	#infoGene<-read.delim(fileGene,header=TRUE,row.names=1,sep="\t")
	namesGenes=read.delim(fileGene,header=FALSE,sep="\t")
	namesGenes<-unlist(as.character(namesGenes[,1]))
}else{
	namesGenes<-c()
}

#######################################################
create_pathway(projet)

# on lit la matrice
if(typeArray == "GPR"){
  
  files<- dir(path=dirFile,pattern=".*\\.gpr$")
  #on change de repertoire
 
  if (all(files != namesFiles))
    	stop("Non correspondance entre les noms du fichiers d'annotation et les noms reels")
	files<-paste(dirFile,files,sep="/")
	dataMA<-read_GPR(files,namesArray)
	
}else if(typeArray=="AG"){
  
  
  files<- dir(path=dirFile,pattern="U.*\\.txt$")
  #on change de repertoire
  
	if (all(files == namesFiles) == FALSE)
    	stop("Non correspondance entre les noms du fichiers d'annotation et les noms reels")

    	files<-paste(dirFile,files,sep="/")
		dataMA<-read_Agilent(namesArray=namesArray,files=files,namesGenes=namesGenes,dye=dye,type="AG")

	if(geneAnnot == TRUE){
		fileName<- paste(projet,"-",nom_fichier,"-rawdataCtrlInfo.txt",sep="")
		fileName<-paste(dirFile,fileName,sep="/")
		tmpinfoGene<-infoGeneAnot[rownames(dataMA),]
		tmpDataMA<-cbind(tmpinfoGene,dataMA)
		write.table(tmpDataMA,fileName,row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)
		rm(tmpDataMA)
	}

	fileName<- paste(projet,"-",nom_fichier,"-rawdataCtrl.txt",sep="")
	fileName<-paste(dirFile,fileName,sep="/")
	write.table(dataMA,fileName,col.names=NA,sep="\t",quote=FALSE);

	dataMA<-delete.control.in.matrix(dataMA,filename=controle)

	if(geneAnnot == TRUE){
		fileName<- paste(projet,"-",nom_fichier,"-rawdataInfo.txt",sep="")
		fileName<-paste(dirFile,fileName,sep="/")
		tmpinfoGene<-infoGeneAnot[rownames(dataMA),]
		tmpDataMA<-cbind(tmpinfoGene,dataMA)
		write.table(tmpDataMA,fileName,col.names=NA,row.names=TRUE,sep="\t",quote=FALSE);
		rm(tmpDataMA)
	}

	fileName<- paste(projet,"-",nom_fichier,"-rawdata.txt",sep="")
	fileName<-paste(dirFile,fileName,sep="/")
	write.table(dataMA,fileName,col.names=NA,sep="\t",quote=FALSE);
	
}else if(typeArray=="NG"){
       
  files<- dir(path=dirFile,pattern=".*\\.pair$")
  #on change de repertoire
  
  if (all(files != namesFiles))
    stop("Non correspondance entre les noms du fichiers d'annotation et les noms reels")
    files<-paste(dirFile,files,sep="/") 
	read_NG(namesEch,files)
}

if(rmArray==TRUE){

	dataMA<-rmArrayByNames(matnum_expre=dataMA,vech_namesArray= arrayRm)
	frameFac<-rmArrayByNames(matnum_expre=frameFac,vech_namesArray = arrayRm)
	print("RM array OK\n")
}



fd<-paste(projet,"-01-stat-descriptive/",sep="")
##connaître la taille de la matrice
#print(dim(dataMA))

#######GRAPHIQUES AVANT NORMALISATION####
##creation du boxplot
print("Raw BoxPlot")

output_name<-paste(fd,projet,"-",nom_fichier,"-raw-boxplot.jpeg",sep="")
create_boxplot(dataMA,output_name)

##Statistiques descriptives
print("Statistiques descriptives")
output_name = paste(fd,projet,"-",nom_fichier,"-raw-stat-des.jpeg",sep="")
create_stats_des(dataMA,output_name)

output_name = paste(fd,projet,"-",nom_fichier,"-raw-statClient.jpeg",sep="")
create_stats_des(dataMA,output_name,minMax=FALSE)
##Correlation
print("Correlation")
cat("** \nCorrelation avant normalisation\n",append=TRUE,file=logNames)
output_name = paste(fd,projet,"-",nom_fichier,"-raw-correlation.jpeg",sep="")
echBadCor<-create_correlation_median(dataMA,output_name)
if(!is.null(echBadCor)){
	cat("echantillons dont la correlation avec le profil median est <0.8",echBadCor,"\n",append=TRUE,file=logNames)
}else{
	cat("OK\n",append=TRUE,file=logNames)
}

####NORMALISATION  (LOWESS)##########
####################################
print("Normalisation")
fd<-paste(projet,"-02-normalisation/",sep="")
dataN<-LOWESS(nom_fichier=nom_fichier,data=dataMA,pngDir=fd,profil.median="NA",graph=1,projet=projet)

if(geneAnnot == TRUE){
	fileName<- paste(fd,"/",projet,"-",nom_fichier,"-rawdataInfo.txt",sep="")
	tmpinfoGene<-infoGeneAnot[rownames(dataN),]
	tmpDataMA<-cbind(tmpinfoGene,dataN)
	write.table(tmpDataMA,fileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE);
	rm(tmpDataMA)

}
fd<-paste(fd,"images/",sep="")
imgNames<-paste(fd,projet,"-norImg.png",sep="")
#images comprenant tous les plot de normalisation 4 échantillons par image (avant apres avant/apres)
montage_cmd<-paste("montage ",fd,"*.png -tile 3x4 -geometry 100% -background none ", imgNames,sep="")
system(montage_cmd)
imgMont<-dir(path=fd,pattern="*-norImg*")
filesNorm<-paste(fd,imgMont,sep="")

### verifier si le script fonctionne toujours avec des sortis gpr
dataN = read.delim(paste(projet,"-02-normalisation/",projet,"-",nom_fichier,"-normalisation.txt",sep=""), sep="\t", header=TRUE, comment.char="",row.names=1)
print("Fin Normalisation")
###################################


###GRAPHIQUE APRES NORMALISATION#####
##creation du boxplot
print("Boxplot Norm")
output_name = paste(fd,projet,"-",nom_fichier,"-lowess.boxplot1.jpeg",sep="")
create_boxplot(dataN,output_name)

##Statistiques descriptives
print("Statistiques descriptives Norm")
output_name = paste(fd,projet,"-",nom_fichier,"-lowess-stat-des.jpeg",sep="")
create_stats_des(dataN,output_name)

output_name = paste(fd,projet,"-",nom_fichier,"-lowess-statClient.jpeg",sep="")
create_stats_des(dataN,output_name,minMax=FALSE)
##Correlation apres normalisation
print("Correlation Norm")
cat("** \nCorrelation apres normalisation\n",append=TRUE,file=logNames)
output_name = paste(fd,projet,"-",nom_fichier,"-lowess-correlation.jpeg",sep="")
echBadCorNorm<-create_correlation_median(dataN,output_name)

if(!is.null(echBadCorNorm)){
	cat("echantillons dont la correlation avec le profil median est <0.8",echBadCorNorm,"\n",append=TRUE,file=logNames)
}else{
	cat("OK\n",append=TRUE,file=logNames)
}


##Génération fichiers TEX stat Descr et corrélation
tex_importData(typeArray,dye,ratio)
tex_norm(projet,filesNorm)
tex_StatDesc(projet,nom_fichier)
tex_Corre(projet,echBadCor,echBadCorNorm,nom_fichier)


if(savingData == TRUE){
	save(frameFacMA,dataMA,file=paste(projet,"-dataRaw.Rdata",sep=""))
}
##############################
#creation d'une matrice reduite
if (clusteringALEA == TRUE){
	print("matrice aleatoire")
	nProbes<-nrow(dataN)*(alea/100)
	sampMatrix<-sampleMatrix(dataN,nProbes)
	sampleMPrefix=paste(projet,"-03-clusterAleatoire/",projet,"-",nom_fichier,alea,"sample",sep="")
	sampleMPrefixShort = paste(projet,"-",nom_fichier,alea,"sample",sep="")
	sampleMName<-paste(sampleMPrefix,".txt",sep="")
	write.table(sampMatrix,sampleMName,quote=FALSE,sep="\t",col.names=NA,row.names=TRUE)

################
###Cluster######
	if(testUnit == FALSE){
	print("debut clustering matrice aleatoire")
		commandCluster<-paste(" cluster -f ",sampleMName," -l  -cg m -g 1 -e 1  -m c",sep="")
		system(commandCluster)
	print("fin clustering matrice aleatoire")
	}else{
		sampMatrix<-read.delim(sampleMName, header=TRUE,sep="\t",row.names=1)
	}
	commandSlcviewMatrix<-paste("slcview.pl ",sampleMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -gtrresolution 0 -arraylabels 0 -atrresolution 0 -o ",sampleMPrefix,"Matrix.png" ,sep="" )
	
	system(commandSlcviewMatrix)
	
	commandSlcviewArray<-paste("slcview.pl ",sampleMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -noimage -o ",sampleMPrefix,"Array.png" ,sep="" )
	
	system(commandSlcviewArray)
	
	commandSlcviewArray<-paste("slcview.pl ",sampleMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -arraylabel 0 -noimage -o ",sampleMPrefix,"ArrayNoName.png" ,sep="" )
	
	system(commandSlcviewArray)
	
	commandSlcviewArray<-paste("slcview.pl ",sampleMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -atrresolution 0 -noimage -o ",sampleMPrefix,"ArrayNoAr.png" ,sep="" )
	
	system(commandSlcviewArray)
	
	
################
###Filtrage######

	nameCDT<-paste(projet,"-03-clusterAleatoire/",projet,"-",nom_fichier,alea,"sample.cdt",sep="")
	print("filtrage matrice aleatoire")
	matSamplecdt<-read.delim(nameCDT,sep="\t")

#ordonne la matrice normalisé
	nG<-as.vector(matSamplecdt[,2])
	nG<-nG[-1:-2]
	nG<-as.character(nG)
	nA<-colnames(matSamplecdt)
	nA<-nA[-1:-4]
	rm(matSamplecdt)
###############
	sampMatrix<-sampMatrix[,nA]
	sampMatrix<-sampMatrix[nG,]
#####################Utilisation de matrix2png pour le clustering de la matrice aleatoire
	facteurName<-paste(projet,"-03-clusterAleatoire/",projet,"-facteur.txt",sep="")
	write.table(frameFac,facteurName,sep="\t",row.names=FALSE,quote=FALSE);
	
	mapName<-paste(projet,"-color.txt",sep="")
	#mapFac<-createMap4matrix2png(frameFac)
	#write.table(mapFac,mapName,sep="\t",row.names=FALSE,quote=FALSE);
	frameFac<-as.data.frame(frameFac)[,nA]
	frameFacN<-frameFac
	ffm2p<-createMatrix2png(frameFac)
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
if(is.null(dim(frameFac))){
	annotFilter<-as.factor(as.character(unlist(frameFac)))
}else{
	annotFilter<-as.factor(as.character(unlist(frameFac[filterParam,])))
}
nbclasses <- nlevels(annotFilter)
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
montageMont<-paste("madProMontage.pl -b ",outfileColor," -m ",projet,"-03-clusterAleatoire/",sampleMPrefixShort,"Matrix.png -t ",projet,"-03-clusterAleatoire/atr.",sampleMPrefixShort,"ArrayNoName.png -g ",filename," -o ",projet,"-03-clusterAleatoire/",projet,"-filtrageCluster.png",sep="")
system(montageMont)
montageTree<-paste("montage ",projet,"-03-clusterAleatoire/atr.",sampleMPrefixShort,"Array.png ",outfileColor," -geometry +0+0 -tile 1x ",projet,"-03-clusterAleatoire/",projet,"-TreeArraycolor.png",sep="")
system(montageTree)
	fileTree=paste(projet,"-03-clusterAleatoire/",projet,"-TreeArraycolor.png",sep="")
	rotateTree<-paste("convert ",fileTree, " -rotate 90 ",fileTree,sep="")
	system(rotateTree)
	}
if(savingData == TRUE){
	save(frameFacN,dataN,sampMatrix,file=paste(projet,"-dataNorm.Rdata",sep=""))
}
####Filtrage matrice totale
print("filtrage matrice totale")
if(is.null(colnames(frameFac))){
	dataN<-dataN[,match(colnames(dataN),names(frameFac))]
}else{
	dataN<-dataN[,match(colnames(dataN),colnames(frameFac))]
}
seuil<-mean(apply(dataN,2,median))
result_filter=filtrage_non_exprimes(dataN,nbclasses,annotFilter,seuil,nval)


m.filtered=dataN[result_filter == TRUE,]

tex_filtrage(projet,annotFilter,seuil,nrow(dataN),nrow(m.filtered),nval)


cat("\nseuil",seuil,file=logNames,sep="\t",append=TRUE)
cat("\nnombre de sonde total",nrow(dataN),file=logNames,sep="\t",append=TRUE)
cat("\nnombre de sonde filtree",nrow(m.filtered),file=logNames,sep="\t",append=TRUE)
filterMPrefix<-paste(projet,"-04-filtre/",projet,"-matrix-filtree",sep="")
filterName<-paste(filterMPrefix,".txt",sep="")


if(ratio != "FALSE"){
  m.filtered<-makeRatio(mat=m.filtered,ratio=ratio,frameFac=frameFac)
}

#filterName<-paste(projet,"-04-filtre/",projet,"-matrix_filtree.txt",sep="")
write.table(m.filtered,filterName,col.names=NA,row.names=TRUE,sep="\t",quote=FALSE)
################Clustering de la matrice totale
if(testUnit==FALSE){
	print("debut clustering matrice totale")
	commandCluster<-paste(" cluster -f ",filterName," -l  -cg m -g 1 -e 1  -m c",sep="")
system(commandCluster)

print("fin clustering matrice totale")
}
	commandSlcviewMatrix<-paste("slcview.pl ",filterMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -gtrresolution 0 -arraylabels 0 -atrresolution 0 -o ",filterMPrefix,"Matrix.png" ,sep="" )
	
	system(commandSlcviewMatrix)
	
	commandSlcviewArray<-paste("slcview.pl ",filterMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -noimage -o ",filterMPrefix,"Array.png" ,sep="" )
	
	system(commandSlcviewArray)
	
	commandSlcviewArray<-paste("slcview.pl ",filterMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -arraylabel 0 -noimage -o ",filterMPrefix,"ArrayNoName.png" ,sep="" )
	
	system(commandSlcviewArray)	
	commandSlcviewArray<-paste("slcview.pl ",filterMPrefix,".cdt -xsize 25 -height 1300 -genelabel 0 -atrresolution 0 -noimage -o ",filterMPrefix,"ArrayNoAr.png" ,sep="" )
	
	system(commandSlcviewArray)
	


filterNamecdt<-paste(projet,"-04-filtre/",projet,"-matrix-filtree.cdt",sep="")
matcdt<-read.delim(filterNamecdt,sep="\t")
#ordonne la matrice normalisé filtre selon le cdt
nG<-as.vector(matcdt[,2])
nG<-nG[-1:-2]
nG<-as.character(nG)
 if(ratio == "FALSE"){
  nA<-colnames(matcdt)
  nA<-nA[-1:-4]
  m.filtered<-m.filtered[,nA]
  frameFac<-as.data.frame(frameFac)[,nA]
  }
 m.filtered<-m.filtered[nG,]
 frameFacF<-frameFac
 rm(matcdt)
#if(is.null(colnames(frameFac))){
#frameFac<-frameFac[match(colnames(m.filtered),names(frameFac))]
#}else{
#frameFac<-frameFac[,match(colnames(m.filtered),colnames(frameFac))]
#}
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
#ffm2p<-createMatrix2png(frameFac)


#outfile<-paste(projet,"-04-filtre/",projet,"-colorSample.png",sep="")
#colorName<-paste("matrix2png -size 24:24  -r -s -dmap ", mapName, " -data " , frameNames ,"  > ", outfile,sep="")
#system(colorName)

#outfile<-paste(projet,"-04-filtre/",projet,"-colorSample2.png",sep="")
#colorName<-paste("matrix2png -size 24:24   -dmap ", mapName, " -data " , frameNames ,"  > ", outfile,sep="")

#system(colorName)
if(geneAnnot == TRUE){
	filterName<-paste(projet,"-04-filtre/",projet,"-matrix_filtreeInfo.txt",sep="")
	tmpinfoGene<-infoGeneAnot[rownames(m.filtered),]
	tmpDataMA<-cbind(tmpinfoGene,m.filtered)
	write.table(tmpDataMA,fileName,col.names=NA,sep="\t",quote=FALSE,row.names=TRUE);
	rm(tmpDataMA)

}

if(savingData == TRUE){
	save(filterParam,ffm2p,m.filtered,frameFac,file=paste(projet,"-dataFilter.Rdata",sep=""))
}
##########test stat
print("test stat")
finalPV <- NULL
finalFC <- NULL
sStrict <- 0.0001
sLache <- 0.001
path<-paste(projet,"-05-student",sep="")

if(Annotation && !is.null(infoGeneAnot$GeneName)){
	pathAnot<-paste(projet,"-06-annotation",sep="")
	filePuce<-paste(pathAnot,"/",projet,"-puce.txt",sep="")
	write.table(infoGeneAnot$GeneName,filePuce,sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
	
}
if(ratio!="FALSE" & bicoul == FALSE){
  
  pvalue<-testLogRatio(mat=m.filtered,projet=projet,path=path)
  pvalFile<-paste(path,"/",projet,"-pvalue",ratio[1],"-",ratio[2],".txt",sep="")
  write.table(pvalue,file=pvalFile,col.names=NA, row.names=TRUE,sep="\t",quote=FALSE)
}else{ 
 
  if(is.null(comparaison)){
	  if(is.null(colnames(frameFac))){
		  comparaison<-combn(levels(as.factor(unlist(frameFac))),2,simplify=TRUE)
	  }else{
		  comparaison<-combn(levels(as.factor(unlist(frameFac[filterParam,]))),2,simplify=TRUE)
	  }
  }
  finalPV <- NULL
  finalFC <- NULL
  sStrict <- 0.0001
  sLache <- 0.001
  #-calcul des Test STAT
  cat("\n**Gene Différentiel***\n\n", file=logNames,append=TRUE)
  cat("Versus\tpval < ",sLache,"\tFDR ",sLache,"\tpval < ",sStrict,"\tFDR ",sStrict,"\tup","\tdown","\n", file=logNames,append=TRUE,sep="")
  path<-paste(projet,"-05-student",sep="")
  tabGenDiff<-matrix(data=NA,ncol=9)
		    nbLache<-paste("NB pval < ",sLache,sep="")
		    FDRLache<-paste("FDR ",sLache,sep="")
		    nbStrict<-paste("NB pval < ",sStrict,sep="")
		    FDRStrict<-paste("FDR ",sStrict,sep="")
  			upLL<-paste("Up ",sLache,sep="")
  			downLL<-paste("Down ",sLache,sep="")
  			upSS<-paste("Up ",sStrict,sep="")
  			downSS<-paste("Down ",sStrict,sep="")
		    colnames(tabGenDiff)<-c("versus",nbLache,FDRLache,nbStrict,FDRStrict,upLL,downLL,upSS,downSS)
		    fileTexCluster<-NULL
  for(i in 1:ncol(comparaison)){
	  #- test stat
  		
	  	versus<-paste(comparaison[1,i],"-Vs-",comparaison[2,i],sep="")
	  	pathDir<-paste(path,"/Comparaison-",versus,sep="")
	  	if(!file.exists(pathDir))
	  		dir.create(pathDir)
	  	result_pval <-pairRows.t.test(comparaison[,i],m.filtered,frameFac,padj.method="none",path=path,graph=TRUE,projet=projet)
	  	fileMatrix <-paste(projet,"-04-filtre/",projet,"-matrix-filtreeMatrix.png",sep="")
	  	fileTree <- paste(projet,"-04-filtre/atr.",projet,"-matrix-filtreeArrayNoName.png ",sep="")
	  	fileOut<-paste(projet,"-05-student/",projet,"-Cluster-",versus,".png",sep="")
	  	fileGraph<-paste(projet,"-05-student/",projet,versus,".png",sep="")
	  	montageMont<-paste("madProMontage.pl -b ",outfileColor," -m ",fileMatrix," -t ",fileTree," -g ",fileGraph," -o ",fileOut,sep="")
		system(montageMont)
	 if(i == 1){
	 	appendFirst = FALSE
	 }else{
	 	appendFirst = TRUE
	 }
	  tex_clusterImage(fileOut,paste("rapport/graphCluster.tex",sep=""),versus,appendFirst,projet)
	 	if(is.null(fileTexCluster)){
	 		fileTexCluster<-paste(versus,".tex",sep="")
	 	}else{
	 		fileTexCluster<-c(fileTexCluster,paste(versus,".tex",sep=""))
	 	}
	  
	  finalPV<-cbind(finalPV,result_pval)
	  colnames(finalPV)[i]<- versus
	  # selection des genes differentiels			
	  pStrict<- subset(result_pval,result_pval <= sStrict)
	  fdrStrict<-(sStrict*nrow(m.filtered))/length(pStrict)
	  if(fdrStrict !="Inf")
	  fdrStrict<-round(fdrStrict,3)
	  
	  pLache <- subset(result_pval,result_pval <= sLache)
	  fdrLache<-(sLache*nrow(m.filtered))/length(pLache)
	  if(fdrLache !="Inf")
	  	fdrLache<-round(fdrLache,3)
	  
	  cat(versus,length(pLache),fdrLache,length(pStrict),fdrStrict,sep="\t",file=logNames,append=TRUE)
	  r<-c(versus,length(pLache),fdrLache,length(pStrict),fdrStrict)	
	  write.table(pLache,file=paste(pathDir,"/",projet,"-",versus,"-",sLache,".txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	  write.table(pStrict,file=paste(pathDir,"/",projet,"-",versus,"-",sStrict,".txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	

	  #-Fold Change 
	  m1<-meanByFact(m.filtered,frameFac,comparaison[1,i])
	  m2<-meanByFact(m.filtered,frameFac,comparaison[2,i])
	  matMean<-cbind(m1,m2)
	  fc<-apply(matMean,1,FC)
	
	  finalFC<-cbind(finalFC,fc)
	  colnames(finalFC)[i]<-paste("FC-",versus,sep="")

	  #- gene up ou down
	  syntheseStat<-cbind(result_pval,fc)
	  upLache<-syntheseStat[which(result_pval<= sLache &  fc >= 0),]
	  downLache<-syntheseStat[which(result_pval<= sLache & fc < 0),]
	  nupL<-nrow(upLache)
	  if(is.null(nupL))
		  nupL<-0
	  ndownL<-nrow(downLache)
	  if(is.null(ndownL))
		  ndownL<-0

	  upStrict<-syntheseStat[which(result_pval<= sStrict &  fc >= 0),]
	  downStrict<-syntheseStat[which(result_pval<= sStrict & fc < 0),]
	  nupS<-nrow(upStrict)
	  if(is.null(nupS))
		  nupS<-0
	  ndownS<-nrow(downStrict)
	  if(is.null(ndownS))
		  ndownS<-0
	  
	  cat("",nupL,ndownL,nupS,ndownS,"\n",sep="\t",file=logNames,append=TRUE)
	  r<-c(r,nupL,ndownL, nupS,ndownS)
	  cat(length(r), "\n")
	  if(is.na(tabGenDiff[1,1])){
	  	tabGenDiff[1,]<-r
	  }else{
	  	#n<-nrow+1
	  	tabGenDiff<-rbind(r,tabGenDiff,deparse.level=0)
	  }
	  colnames(syntheseStat)[1]<-paste("Pvalue-",versus,sep="")
	  colnames(syntheseStat)[2]<-paste("FoldChange-",versus,sep="")
	  #-fichier de synthese
	  write.table(upLache,file=paste(pathDir,"/",projet,"-",versus,"-up.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
	  write.table(downLache,file=paste(pathDir,"/",projet,"-",versus,"-down.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	

	#Fichiers pour l'annotation de toutes les listes
	if(Annotation && !is.null(infoGeneAnot$GeneName)){

		fileList<-paste(pathAnot,"/",projet,"-listeFile.txt",sep="")
		upAnotFile<-paste(pathAnot,"/",projet,"-",versus,"-upAnot.txt",sep="")
		downAnotFile<-paste(pathAnot,"/",projet,"-",versus,"-downAnot.txt",sep="")
		cat(upAnotFile,"\n",downAnotFile,file=fileList,append=TRUE,sep="")
		GeneName<-infoGeneAnot$GeneName
		names(GeneName)<-rownames(infoGeneAnot)
		upAnot<-GeneName[rownames(upLache)]
		downAnot<-GeneName[rownames(downLache)]
		write.table(upAnot,file=upAnotFile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep="\t")
		write.table(downAnot,file=downAnotFile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep="\t")


	}


	  write.table(syntheseStat,file=paste(pathDir,"/",projet,"-",versus,"-allSynth.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
				
  }
	tex_question(fileTexCluster,paste("rapport/graphCluster.tex",sep=""))
  write.table(finalPV,file=paste(path,"/",projet,"-allpval.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
  write.table(finalFC,file=paste(path,"/",projet,"-allFC.txt",sep=""),row.names=TRUE,col.names=NA,sep="\t",quote=FALSE)	
  tex_genDiff(tabGenDiff)
	if(Annotation){
		resultDir<-paste(pathAnot,"/resultat",sep="")
		commandAnnotation<-paste("gominer -p ",filePuce," -f ",fileList, " -s ", species, " -r ", resultDir,sep="")
		system(commandAnnotation)
		filesGominer<-dir(path=resultDir, pattern="S_*")

		
	}
	print("FIN")

}
	  
}



