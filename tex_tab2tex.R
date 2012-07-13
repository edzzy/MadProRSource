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
