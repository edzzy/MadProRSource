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
`delete.control.in.matrix` <-
function(rawData,filename="controles.txt"){
# Elimination des controles
ctrl=read.delim(filename,header=FALSE)
ctrl=as.vector(unlist(ctrl))
genes=as.vector(unlist(row.names(rawData)))
noCtrlData<-rawData[-match(ctrl,genes),]
 return (noCtrlData)

}

sampleMatrix <-function(mat,n){
  sampleMat<- mat[sample(1:dim(mat)[1],n),]
return(sampleMat)
}


`create_level_matrix` <-
function(m,classech){
  val=levels(classech)
    L=list()
  for (i in 1:length(val)) {
      L=c(L,list(m[,which(classech==levels(classech)[i])]))
    }
  names(L)= val
    return (L)
}

meanByFact<-function(mat,fac,l){
	mat<-as.matrix(mat)
#fac<-as.factor(unlist(fac))
fac<-as.matrix(fac)
matFac<-mat[,which(fac == as.character(l),arr.ind=TRUE)[,2]]
meanFac<-apply(matFac,1,mean)
return(meanFac)

}
