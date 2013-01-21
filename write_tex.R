#-- Tex Output


tex_annotGominer<-function(resultDir,fileNamesGominer){
filesGominer<-dir(path=resultDir,pattern="^S_*")
for(i in 1:length(filesGominer)){
		tmpFiles<-paste(resultDir,"/",filesGominer[i],sep="")
		tex_tab2tex(tmpFiles,fileNamesGominer,title=filesGominer[i])
	}
	return()
}


tex_clusterImage<-function(fileCluster,tex_file,versus,appendFirst,projet){
	
	 section<-paste("\\subsubsection*{Comparaison : ", versus,"}\n")
	cat(section,file=tex_file,append=appendFirst)
	cat("\\begin{figure}[H]\n
  	\\centering\n
  	\\begin{tabular}{c}\n
  	",file=tex_file,append=TRUE)
    
  	clusterImg<-paste("\\includegraphics[scale=0.20]{../",fileCluster,"} \\\\\n",sep="")
  	clusterTree<-paste("\\includegraphics[scale=0.20]{../",projet,"-04-filtre/",projet,"-TreeArraycolor.png} \\\\\n",sep="")  
  	clusterLegend<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/Legende-",projet,"-colorSample.png}\\\\\n",sep="")
	cat(clusterImg,clusterLegend,file=tex_file,append=TRUE)
	cat("\\end{tabular}\n
  	\\end{figure}\n
  	",file=tex_file,append=TRUE)
		
}





tex_Corre<-function(projet,echBadCor,echBadCorNorm,dirName="."){

	fileName<-paste(dirName,"/graphCorrelation.tex",sep="")
  
	cat("\\begin{figure}[H]\n                                                                                                 
    \\centering\n
    \\begin{tabular}{cc}\n",file=fileName,append=FALSE)
     include1=paste("\\includegraphics[scale=0.20]{../",projet,"-01-stat-descriptive/",projet,"-raw-correlation.jpeg} &\n",sep="")
    include2=paste("\\includegraphics[scale=0.20]{../",projet,"-02-normalisation/images/",projet,"-lowess-correlation.jpeg} \\\\\n",sep="")
     cat(include1,include2,file=fileName,append=TRUE)
     cat("(a) & (b) \\\\\n
    \\end{tabular}\n
        \\caption{\\label{Correlation}Corrélation de chaque échantillon par rapport au profil médian avant (a) et après (b) normalisation}\n
\\end{figure}\n", file=fileName,append=TRUE)
  
  if(length(echBadCor)!=0){
    cat("Echantillons dont la corrélation est inférieur à 0.8 avant la normalisation :\n",file=fileName,append=TRUE)
    cat("\\begin{itemize}\n",file=fileName,append=TRUE)
    for(i in 1:length(echBadCor)){
      cat("\\item ",echBadCor[i], "\n",file=fileName,append=TRUE)
    }
    cat("\\end{itemize}\n",file=fileName,append=TRUE)
  }else{
    cat("Les échantillons avant Normalisation ont une bonne corrélation avec le profil médian \\\\\n" ,file=fileName,append=TRUE)
  }
  
  if(length(echBadCorNorm)!=0){
    cat("Echantillons dont la corrélation est inférieur à 0.8 après la normalisation : \n",file=fileName,append=TRUE)
    cat("\\begin{itemize}\n",file=fileName,append=TRUE)
    for(i in 1:length(echBadCorNorm)){
      cat("\\item ",echBadCorNorm[i],"\n", file=fileName,append=TRUE)
    }
    cat("\\end{itemize}\n",file=fileName,append=TRUE)
    }else{
      cat("Les échantillons après Normalisation ont une bonne corrélation avec le profil médian \\\\\n", file=fileName,append=TRUE)
    }
}



tex_filtrage<-function(projet,annotFilter,seuil,initialProbe,conserveProbe, nval,dirName="."){
	fileName<-paste(dirName,"/filtrage.tex",sep="")
  
  cat("\\begin{description}\n",file=fileName,append=FALSE)
  textSeuil<-paste("\\item [seuil : ]moyenne de la médiane des échantillons. Pour l'étude le seuil est de : ", seuil,sep="")
  cat(textSeuil,file=fileName,append=TRUE)
  filtreRule<-paste("\\item[Règles de filtrage : ] \n
                    \\begin{itemize} \n
                    \\item Le filtrage est effecué sur les ", nlevels(annotFilter), "classes suivantes\n")
                    cat(filtreRule,file=fileName,append=TRUE)
                    cat("\\begin{itemize}\n",file=fileName,append=TRUE)
                      for (i in 1:nlevels(annotFilter)){
    
                        classe<-paste("\\item ", levels(annotFilter)[i],"\n",sep="")
                        cat(classe,file=fileName,append=TRUE)
    
                      }
                    cat("\\end{itemize}\n",file=fileName,append=TRUE)
                    r2<-paste("\\item une sonde est conservée si au moins ",nval," \\% des échantillons d'une classe passe le seuil\n",sep="")
                nbSonde<-paste("\\item Nombre de sonde conservées dans l'analyse : ",conserveProbe, " sur ", initialProbe,"\n",sep="")
         cat(r2,file=fileName,append=TRUE)
        cat(nbSonde,file=fileName,append=TRUE)             
  cat("\\end{itemize}\n",file=fileName,append=TRUE)
  cat("\\end{description}\n",file=fileName,append=TRUE)
  
  cat("\\begin{figure}[H]\n
  	\\centering\n
  	\\begin{tabular}{c}\n
  	",file=fileName,append=TRUE)
    
  	clusterFile<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/",projet,"-filtrageCluster.png} \\\\\n",sep="")
  	clusterLegend<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/Legende-",projet,"-colorSample.png}\\\\\n",sep="")
	clusterTree<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/",projet,"-TreeArraycolor.png} \\\\\n",sep="")  
  
  cat(clusterFile,clusterLegend,clusterTree,file=fileName,append=TRUE)
  cat("\\end{tabular}\n
  	\\end{figure}\n
  	",file=fileName,append=TRUE)
}



tex_genDiff<-function(tabGenDiff,projet,dirName=".",file="genDiff.tex"){
	fileName<-paste(dirName,"/",file,sep="")
	
	header<-paste(colnames(tabGenDiff)[1:3]," & ", sep="")
	header<-c(header, colnames(tabGenDiff)[4], " \\\\ \n \\hline\n")
	
	if(nrow(tabGenDiff) != 1){	
		tabGenDiff<-tabGenDiff[sort(as.numeric(tabGenDiff[,2]),index.return=TRUE,decreasing=TRUE)$ix,]
	}	

	cat("\\begin{tabular}{l|r|ll}\n",file=fileName,append=FALSE)
	cat(header,file=fileName,append=TRUE)
	for(i in 1:nrow(tabGenDiff)){
		versus<-tabGenDiff[i,1]
		n<-tabGenDiff[i,2]
		
		#versus<-tabGenDiff[i,1]
		#nStrict<-tabGenDiff[i,4]
		#FDRSctrict<-tabGenDiff[i,5]
		
		up<-tabGenDiff[i,3]
		down<-tabGenDiff[i,4]
		
		ligne<-c(versus, n, up)
		ligne<-paste(ligne, " & ", sep="")
		ligne<-c(ligne, down," \\\\ \n")
		cat(ligne,file=fileName,append=TRUE)
		
	}
	cat("\\hline\n\\end{tabular}\\par \n",file=fileName,append=TRUE)
	
}


tex_importData<-function(typeArray,dye,dirName="."){
fileName<-paste(dirName,"/importData.tex",sep="") 
	puce<-NULL
	color<-NULL
	scan<-NULL
	file<-NULL
	colo<-NULL
	ratioText<-NULL
	if(typeArray == "AG"){
		puce<-"Agilent"
		scan<-"Feature Extraction"
		file<-"txt"
	if(dye == 2 ){
		color<-"Pseudo-MonoCouleur"
		colo<-"des colonnes gMedianSignal et rMedianSignal.\n"
	}else if(dye == 1){
		color<-"MonoCouleur"
		colo<-"de la colonne gMedianSignal."
	}
#else if(dye == 2 & ratio != "FALSE"){
#		color<-"Bicouleur et ratio"
#		colo<-"des colonnes gMedianSignal et rMedianSignal.\n"
#		ratioText<-paste("Pour l'analyse en ratio, les rapports ont été fait de la façon suivante : $\\frac{",ratio[1],"}{",ratio[2],"}$")
#	}
	}else if( typeArray == "NG"){
		puce<-"NimbelGen"
		scan<-"NimbelScan"
		file<-".pair"
		colo<-"de la colonne PM.\n"
	}else if(typeArray == "GPR"){
		puce<-"GenePix"
		scan<-"GenePix"
		file<-".gpr"
		colo<-"des colonnes F635 Median et F532 Median.\n"
  
	}	


	arrayType<-paste("\\subsection*{",puce,"}\n",sep="")



	scanType<-paste("\\emph{",scan,"}",sep="")
	paragraphe<-arrayType

	if(typeArray == "AG"){
		colorType<-paste("\\subsubsection*{",color,"}\n",sep="")
		paragraphe<-paste(paragraphe,colorType,sep="")
	}

	paragraphe<-paste(paragraphe,"La matrice d'expression est obtenue à partir des données issues du logiciel de scanning ",scanType,".\n", 
				"Chaque fichier ",file," comporte le signal de ",dye, " dye.\n",
				"\\par\n
				Le signal utilisé pour la matrice d'expression est celui ",colo,
				"\\\\Les sondes contrôles sont retirées lors de l'analyse.\n
				\\par\n",sep="")


	cat(paragraphe,file=fileName,append=FALSE)


}




tex_norm<-function(projet,filesNorm,dirName="."){

 fileName<-paste(dirName,"/graphNorm.tex",sep="") 

  cat("\\setlongtables\n
    \\begin{longtable}{cc}\n", file = fileName,append=FALSE)
  include<-"\\includegraphics[scale=0.15]{"
  carac<-"}& \n"
  for(i in 1:length(filesNorm)){
	  modI<- i %% 2
      if (modI  == 0){
        carac<-"} \\\\\n"
      }else{
		  if(i ==length(filesNorm)){
        carac<-"}  & \\\\\n"      
		  }else{
        carac<-"} &\n"      
			  
		}
      }
      includeTex<-paste(include,"../",filesNorm[i],carac,sep="")
      cat(includeTex,file=fileName,append=TRUE)
    }
  cat("
        \\caption{\\label{Norm}Graphiques des distributions des échantillons avant et après normalisation contre le profil médian et graphique des distribution avant/après d'un même échantillon}\n
\\end{longtable}\n",
  file=fileName, append=TRUE)
}


tex_question<-function(vectFile,texFile){
	
	for(i  in 1:length(vectFile)){
		text<-paste("\\input{",vectFile[i],"}\n",sep="")
		if(i == 1){
			cat(text,file=texFile,append=FALSE)
		}else{
			cat(text,file=texFile,append=TRUE)
		}
	}
	
}

tex_StatDesc<-function(projet,dirName="."){

	fileName=paste(dirName,"/graphStatDesc.tex",sep="")

	cat("\\begin{figure}[H]\n
    \\centering\n
    \\begin{tabular}{cc}\n", file = fileName)
      include1<-paste("\\includegraphics[scale=0.20]{../",projet,"-01-stat-descriptive/",projet,"-raw-statClient} &\n",sep="")
      include2<-paste("\\includegraphics[scale=0.20]{../",projet,"-02-normalisation/images/",projet,"-lowess-statClient.jpeg} \\\\\n",sep="")
      cat(include1, include2,file=fileName, append=TRUE) 
        cat("(a) & (b) \\\\\n 
    \\end{tabular}\n
        \\caption{\\label{statDes}Statistiques descriptives avant (a) et après (b) normalisation}\n
\\end{figure}\n",
  file=fileName, append=TRUE)
}

tex_boxplot<-function(projet="",dirName="."){
	fileName=paste(dirName,"/graphStatDesc.tex",sep="")
  cat("\\begin{figure}[H]\n
    \\centering\n
    \\begin{tabular}{cc}\n", file = fileName)
      include1<-paste("\\includegraphics[scale=0.20]{../",projet,"-01-stat-descriptive/",projet,"-raw-boxplot} &\n",sep="")
      include2<-paste("\\includegraphics[scale=0.20]{../",projet,"-02-normalisation/images/",projet,"-lowess-boxplot.jpeg} \\\\\n",sep="")
      cat(include1, include2,file=fileName, append=TRUE) 
        cat("(a) & (b) \\\\\n 
    \\end{tabular}\n
        \\caption{\\label{statDes}Statistiques descriptives avant (a) et après (b) normalisation}\n
\\end{figure}\n",
  file=fileName, append=TRUE)
}

tex_tab2tex<-function(fileGominer,fileName,title,n=10,append=TRUE){
#Fonction qui transforme un fichier tabule gominer en fichier de sortie LaTex avec les n premieres lignes

	gf<-read.delim(fileGominer,header=TRUE,sep="\t",row.names=NULL,nrow=n,stringsAsFactors=FALSE)
	cat("\\subsubsection*{\\small{",title,"}}\n",file=fileName,append=append)	
	if(nrow(gf) != 0){
	nH<-ncol(gf)
	aH<-nH - 1
	pH<-nH+1	
	header<-paste(colnames(gf)[1:aH], " & ", sep="")
	header<-c(header,colnames(gf)[nH], " \\\\ \n \\hline\n")
	cat(c("\\begin{tabular}{|",rep("l|",aH),"p{5cm}|}\n \\hline \n"),file=fileName, append=TRUE)
	cat(header,file=fileName,append=TRUE)
	for(i in 1:nrow(gf)){
		line<-paste(gf[i,1:aH], " & ", sep="")
		line<-c(line,gf[i,nH], " \\\\ \n \\hline\n")
		cat(line,file=fileName,append=TRUE)

	}
	cat(c("\\end{tabular} \\par \n"),file=fileName, append=TRUE)
	}else{
		
	cat("Pas d'annotation fonctionelle \\par",file=fileName,append=append)	
	}
}
