tex_genDiff<-function(tabGenDiff,projet,nom_fichier){
	
	fileName="rapport/genDiff.tex"
	headerS<-paste(colnames(tabGenDiff)[1:3]," & ",sep="")
	headerS<-c(headerS, colnames(tabGenDiff)[6]," & ", colnames(tabGenDiff)[7], " \\\\ \n \\hline\n")
	
	if(nrow(tabGenDiff) != 1){	
		tabGenDiff<-tabGenDiff[sort(as.numeric(tabGenDiff[,2]),index.return=TRUE,decreasing=TRUE)$ix,]
	}	

	cat("\\begin{tabular}{l|ll|ll}\n",file=fileName,append=FALSE)
	cat(headerS,file=fileName,append=TRUE)
	for(i in 1:nrow(tabGenDiff)){
		versus<-tabGenDiff[i,1]
		nLache<-tabGenDiff[i,2]
		FDRLache<-tabGenDiff[i,3]
		
		#versus<-tabGenDiff[i,1]
		#nStrict<-tabGenDiff[i,4]
		#FDRSctrict<-tabGenDiff[i,5]
		
		upL<-tabGenDiff[i,6]
		downL<-tabGenDiff[i,7]
		
		ligne<-c(versus, nLache, FDRLache,upL)
		ligne<-paste(ligne, " & ", sep="")
		ligne<-c(ligne, downL," \\\\ \n")
		cat(ligne,file=fileName,append=TRUE)
		
	}
	cat("\\hline\n\\end{tabular}\\par \n",file=fileName,append=TRUE)
	
	header1<-paste(colnames(tabGenDiff)[1]," & ",sep="")
	header2<-paste(colnames(tabGenDiff)[4:5]," & ",sep="")
	headerS<-c(header1,header2, colnames(tabGenDiff)[8]," & ", colnames(tabGenDiff)[9], " \\\\ \n \\hline\n")
	
	if(nrow(tabGenDiff) != 1){	
		tabGenDiff<-tabGenDiff[sort(as.numeric(tabGenDiff[,4]),index.return=TRUE,decreasing=TRUE)$ix,]
	}	
	cat("\\begin{tabular}{l|ll|ll}\n",file=fileName,append=TRUE)
	cat(headerS,file=fileName,append=TRUE)
	for(i in 1:nrow(tabGenDiff)){
		versus<-tabGenDiff[i,1]
		nLache<-tabGenDiff[i,4]
		FDRLache<-tabGenDiff[i,5]
		
		#versus<-tabGenDiff[i,1]
		#nStrict<-tabGenDiff[i,4]
		#FDRSctrict<-tabGenDiff[i,5]
		
		upL<-tabGenDiff[i,8]
		downL<-tabGenDiff[i,9]
		
		ligne<-c(versus, nLache, FDRLache,upL)
		ligne<-paste(ligne, " & ", sep="")
		ligne<-c(ligne, downL," \\\\ \n")
		cat(ligne,file=fileName,append=TRUE)
		
	}
	cat("\\hline\\end{tabular}\n",file=fileName,append=TRUE)
}
