tex_clusterImage<-function(fileCluster,tex_file,versus,appendFirst,projet){
	
 section<-paste("\\subsubsection*{Comparaison : ,", versus,"}\n")
	cat(section,file=tex_file,append=appendFirst)
	cat("\\begin{figure}[H]\n
  	\\centering\n
  	\\begin{tabular}{c}\n
  	",file=tex_file,append=TRUE)
    
  	clusterImg<-paste("\\includegraphics[scale=0.20]{../",fileCluster,"} \\\\\n",sep="")
  	clusterTree<-paste("\\includegraphics[scale=0.20]{../",projet,"-04-filtre/",projet,"-TreeArraycolor.png} \\\\\n",sep="")  
  	clusterLegend<-paste("\\includegraphics[scale=0.20]{../",projet,"-03-clusterAleatoire/Legende-sample.png}\\\\\n",sep="")
  cat(clusterImg,clusterLegend,file=tex_file,append=TRUE)
  cat("\\end{tabular}\n
  	\\end{figure}\n
  	",file=tex_file,append=TRUE)
		
}
