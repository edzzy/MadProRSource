makeRatio<-function(mat,frameFac,ratio){
  
  mat<-mat[,colnames(frameFac)]
  
  EchMat<-mat[,which(frameFac[1,] == ratio[1])]
  RefMat<-mat[,which(frameFac[1,] == ratio[2])] 
  EchFrame<-frameFac[,which(frameFac[1,] == ratio[1])]
  RefFrame<-frameFac[,which(frameFac[1,] == ratio[2])]
  
  EchFrame<-EchFrame[,order(EchFrame[2,])]
  RefFrame<-RefFrame[,order(RefFrame[2,])]
  
  EchMat<-EchMat[,colnames(EchFrame)]
  RefMat<-RefMat[,colnames(RefFrame)]
  
  matRatio<-EchMat/RefMat
  namesRatio<-paste(colnames(EchMat),colnames(RefMat),sep="/")
  colnames(matRatio)<-namesRatio
  return(matRatio)
}