`order_by_cdt` <-
function(inputm,cdt){
  #Lecture du .cdt pour récupérer le vecteur des noms de genes dans l'ordre du clustering
  #cdt=read.table(cdtfile,skip=3,sep="\t")
  vecRef=as.vector(unlist(cdt[,2]))

  new_m=inputm[vecRef,]
  return (new_m)
}

