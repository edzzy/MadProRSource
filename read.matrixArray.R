`read.matrixArray` <-
function(file){
	mat<-read.delim(file, sep="\t")
	row.names(mat)<-mat[,1]
	mat<-mat[,-1]
	return(mat)

}

