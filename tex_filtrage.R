tex_filtrage<-function(projet,annotFilter,seuil,initialProbe,conserveProbe, nval){
  
  cat("\\begin{description}\n",file="rapport/filtrage.tex",append=FALSE)
  textSeuil<-paste("\\item [seuil : ]moyenne de la médiane des échantillons. Pour l'étude le seuil est de : ", seuil,sep="")
  cat(textSeuil,file="rapport/filtrage.tex",append=TRUE)
  filtreRule<-paste("\\item[Règles de filtrage : ] \n
                    \\begin{itemize} \n
                    \\item Le filtrage est effecué sur les ", nlevels(annotFilter), "classes suivantes\n")
                    cat(filtreRule,file="rapport/filtrage.tex",append=TRUE)
                    cat("\\begin{itemize}\n",file="rapport/filtrage.tex",append=TRUE)
                      for (i in 1:nlevels(annotFilter)){
    
                        classe<-paste("\\item ", levels(annotFilter)[i],"\n",sep="")
                        cat(classe,file="rapport/filtrage.tex",append=TRUE)
    
                      }
                    cat("\\end{itemize}\n",file="rapport/filtrage.tex",append=TRUE)
                    r2<-paste("\\item une sonde est conservée si au moins ",nval," \\% des échantillons d'une classe passe le seuil\n",sep="")
                nbSonde<-paste("\\item Nombre de sonde conservées dans l'analyse : ",conserveProbe, " sur ", initialProbe,"\n",sep="")
         cat(r2,file="rapport/filtrage.tex",append=TRUE)
        cat(nbSonde,file="rapport/filtrage.tex",append=TRUE)             
  cat("\\end{itemize}\n",file="rapport/filtrage.tex",append=TRUE)
  cat("\\end{description}\n",file="rapport/filtrage.tex",append=TRUE)
  
  cat("\\begin{figure}[H]\n
  	\\centering\n
  	\\begin{tabular}{c}\n
  	",file="rapport/filtrage.tex",append=TRUE)
    
  	clusterFile<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/",projet,"-filtrageCluster.png} \\\\\n",sep="")
  	clusterLegend<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/Legende-sample.png}\\\\\n",sep="")
	clusterTree<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/",projet,"-TreeArraycolor.png} \\\\\n",sep="")  
  
  cat(clusterFile,clusterLegend,clusterTree,file="rapport/filtrage.tex",append=TRUE)
  cat("\\end{tabular}\n
  	\\end{figure}\n
  	",file="rapport/filtrage.tex",append=TRUE)
}