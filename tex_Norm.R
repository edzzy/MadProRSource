tex_norm<-function(projet,filesNorm){
  
  cat("\\setlongtable\n
    \\begin{longtable}{cc}\n", file = "rapport/graphNorm.tex",append=FALSE)
  include<-"\\includegraphics[scale=0.15]{"
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
  cat("
        \\caption{\\label{Norm}Graphiques des distributions des échantillons avant et après normalisation contre le profil médian et graphique des distribution avant/après d'un même échantillon}\n
\\end{longtable}\n",
  file="rapport/graphNorm.tex", append=TRUE)
}
