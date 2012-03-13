tex_norm<-function(projet,filesNorm){
  
  cat("\\begin{figure}[H]\n
    \\centering\n
    \\begin{tabular}{cc}\n", file = "rapport/graphNorm.tex",append=FALSE)
  include<-"\\includegraphics[scale=0.10]{"
  carac<-"}& \n"
  for(i in 1:length(filesNorm)){
      if (i %% 2 == 0){
        carac<-"} \\\\\n"
      }else{
        carac<-"} &\n"      
      }
      includeTex<-paste(include,"../",filesNorm[i],carac,sep="")
      cat(includeTex,file="rapport/graphNorm.tex",append=TRUE)
    }
  cat("\\end{tabular}\n
        \\caption{\\label{Norm}Graphiques des distributions des échantillons avant et après normalisation contre le profil médian et graphique des distribution avant/après d'un même échantillon}\n
\\end{figure}\n",
  file="rapport/graphNorm.tex", append=TRUE)
}