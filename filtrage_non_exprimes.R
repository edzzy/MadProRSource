`filtrage_non_exprimes` <-
function (mat.cdt,nbclasses,annotEch,seuil,nbval)
{
## Fonction de filtrage #########################
### Parametres de la fonction : 
##### 1 : matrice ordonnée dans l'ordre du clustering
##### 2 : nombre de classes d'échantillon. =1 si on faire un filtrage global sur l'ensemble des valeurs
##### 3 : fichier d'annotation des échantillon. Peut-être NA si filtrage global
##### 4 : seuil choisi
##### 5 : pourcentage de valeur qui doivent passer le seuil
 if(nbclasses==1){
     classes.matrix=list(mat.cdt)
     summary(classes.matrix)
  }
  else{
    #annot_ech=read.delim(annotFile)  
    classech=annotEch
    
    classes.matrix=create_level_matrix(mat.cdt,classech)
    summary(classes.matrix)
  }
  # On regarde le nombre de valeurs dépassant le seuil dans chaque classe d'échantillon, et on garde la sonde si on a la moitié des valeurs
  current_filter= rep(FALSE, dim(classes.matrix[[1]])[1])
  
  n=nbval/100
  for(i in 1:nbclasses){
     filtre = kOverA(dim(classes.matrix[[i]])[2]*n,seuil)
     result_filter = genefilter(classes.matrix[[i]],filtre)
     current_filter= result_filter | current_filter
  }
  return(current_filter)
}

