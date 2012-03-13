`doAllImageArray` <-
function(path="./",fileMap="map.txt",fileimg="output"){
	cdtFiles<-dir(path=path,pattern=".\\.cdt")
	prefFiles<-sub("\\.cdt","",cdtFiles)
	frameFiles<-paste(prefFiles,"-frame.txt",sep="")

	for (i in 1:length(cdtFiles)){
		#matrice du cdt
		matCDT<-read.delim(cdtFiles[i],sep="\t")
		matArray<-read.delim(frameFiles[i], row.names=1,header=TRUE)
		matArray<-as.matrix(matArray)
		#ordonne la matrice normalisé
		nG<-as.vector(matCDT[,2])
		nG<-nG[-1:-2]
		nA<-colnames(matCDT)
		nA<-nA[-1:-4]
		matCDT<-matCDT[,-1:-4]
#		rm(matCDT)
		matArray<-matArray[,nA]
		fileArray<-paste(prefFiles[i],"aFrame.txt",sep="")
		fileimg<-prefFiles[i]
		fileMap<-paste(prefFiles[i],"Map.txt",sep="")
		arrayCDTimage(matCDT,matArray,fileArray=fileArray, fileMap=fileMap, fileimg=fileimg,map=TRUE)
		
	}	
}

