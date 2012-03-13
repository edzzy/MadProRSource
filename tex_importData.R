tex_importData<-function(typeArray,dye,ratio){
  
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
  if(dye == 2 & ratio == "FALSE"){
    color<-"Pseudo-MonoCouleur"
    colo<-"des colonnes gMedianSignal et rMedianSignal.\n"
  }else if(dye == 1){
    color<-"MonoCouleur"
    colo<-"de la colonne gMedianSignal."
  }else if(dye == 2 & ratio != "FALSE"){
    color<-"Bicouleur et ratio"
    colo<-"des colonnes gMedianSignal et rMedianSignal.\n"
    ratioText<-paste("Pour l'analyse en ratio, les rapports ont été fait de la façon suivante : $\\frac{",ratio[1],"}{",ratio[2],"}$")
  }
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

if(!is.null(ratio)){
  paragraphe<-paste(paragraphe,ratioText,sep="")
}

cat(paragraphe,file="rapport/importData.tex",append=FALSE)
}