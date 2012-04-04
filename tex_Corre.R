tex_Corre<-function(projet,echBadCor,echBadCorNorm,nom_fichier){
  
  cat("\\begin{figure}[H]\n                                                                                                 
    \\centering\n
    \\begin{tabular}{cc}\n",file="rapport/graphCorrelation.tex",append=FALSE)
        include1=paste("\\includegraphics[scale=0.20]{../",projet,"-01-stat-descriptive/",projet,"-",nom_fichier,"-raw-correlation.jpeg} &\n",sep="")
        include2=paste("\\includegraphics[scale=0.20]{../",projet,"-02-normalisation/images/",projet,"-",nom_fichier,"-lowess-correlation.jpeg} \\\\\n",sep="")
        cat(include1,include2,file="rapport/graphCorrelation.tex",append=TRUE)
        cat("(a) & (b) \\\\\n
    \\end{tabular}\n
        \\caption{\\label{Correlation}Corrélation de chaque échantillon par rapport au profil médian avant (a) et après (b) normalisation}\n
\\end{figure}\n", file="rapport/graphCorrelation.tex",append=TRUE)
  
  if(length(echBadCor)!=0){
    cat("Echantillons dont la corrélation est inférieur à 0.8 avant la normalisation :\n",file="rapport/graphCorrelation.tex",append=TRUE)
    cat("\\begin{itemize}\n",file="rapport/graphCorrelation.tex",append=TRUE)
    for(i in 1:length(echBadCor)){
      cat("\\item ",echBadCor[i], "\n",file="rapport/graphCorrelation.tex",append=TRUE)
    }
    cat("\\end{itemize}\n",file="rapport/graphCorrelation.tex",append=TRUE)
  }else{
    cat("Les échantillons avant Normalisation ont une bonne corrélation avec le profil médian \\\\\n" ,file="rapport/graphCorrelation.tex",append=TRUE)
  }
  
  if(length(echBadCorNorm)!=0){
    cat("Echantillons dont la corrélation est inférieur à 0.8 après la normalisation : \n",file="rapport/graphCorrelation.tex",append=TRUE)
    cat("\\begin{itemize}\n",file="rapport/graphCorrelation.tex",append=TRUE)
    for(i in 1:length(echBadCorNorm)){
      cat("\\item ",echBadCorNorm[i],"\n", file="rapport/graphCorrelation.tex",append=TRUE)
    }
    cat("\\end{itemize}\n",file="rapport/graphCorrelation.tex",append=TRUE)
    }else{
      cat("Les échantillons après Normalisation ont une bonne corrélation avec le profil médian \\\\\n", file="rapport/graphCorrelation.tex",append=TRUE)
    }
}
