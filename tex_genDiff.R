tex_genDiff<-function(tabGenDiff,projet,fileName="rapport/genDiff.tex"){
	
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
