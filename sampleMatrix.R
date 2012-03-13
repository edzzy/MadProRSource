`sampleMatrix` <-
function(mat,n){
  sampleMat<- mat[sample(1:dim(mat)[1],n),]
	return(sampleMat)
}

