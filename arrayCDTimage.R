`arrayCDTimage` <-
function(matCDT,matArray,fileArray="array.txt", fileMap="map.txt", fileimg="output",map=TRUE){
#	matCDT<-as.matrix(matCDT)
#	matArray<-as.factor(matArray)

	#check colonnes
	if(!all(colnames(matCDT) == colnames(matArray))){
			matArray<-matArray[,colnames(matCDT)]
		}
	if(map){
		mapFac<-createMap4matrix2png(matArray)
		write.table(mapFac,fileMap,sep="\t",row.names=FALSE,quote=FALSE);
	}
	ffm2p<-createMatrix2png(matArray)
	write.table(ffm2p,fileArray,sep="\t",row.names=FALSE,quote=FALSE);
	colorName<-paste("matrix2png -size 24:12  -r -s -dmap ", fileMap, " -data " , fileArray ,"  > ",fileimg,".png" ,sep="")
	system(colorName)
	colorName<-paste("matrix2png -size 24:12  -r -dmap ", fileMap, " -data " , fileArray ,"  > ",fileimg,"nS.png" ,sep="")
	system(colorName)

}

