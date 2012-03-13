`delete.control.in.matrix` <-
function(rawData,filename="controles.txt"){
# Elimination des controles
  ctrl=read.delim(filename,header=FALSE)
  ctrl=as.vector(unlist(ctrl))
  genes=as.vector(unlist(row.names(rawData)))
  noCtrlData<-rawData[-match(ctrl,genes),]

  return (noCtrlData)

}

