tex_StatDesc<-function(projet,nom_fichier){
  cat("\\begin{figure}[H]\n
    \\centering\n
    \\begin{tabular}{cc}\n", file = "rapport/graphStatDesc.tex")
      include1<-paste("\\includegraphics[scale=0.20]{../",projet,"-01-stat-descriptive/",projet,"-",nom_fichier,"-raw-statClient} &\n",sep="")
      include2<-paste("\\includegraphics[scale=0.20]{../",projet,"-02-normalisation/images/",projet,"-",nom_fichier,"-lowess-statClient.jpeg} \\\\\n",sep="")
      cat(include1, include2,file="rapport/graphStatDesc.tex", append=TRUE) 
        cat("(a) & (b) \\\\\n 
    \\end{tabular}\n
        \\caption{\\label{statDes}Statistiques descriptives avant (a) et après (b) normalisation}\n
\\end{figure}\n",
  file="rapport/graphStatDesc.tex", append=TRUE)
}