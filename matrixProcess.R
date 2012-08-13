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

