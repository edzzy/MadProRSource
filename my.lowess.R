`my.lowess` <-
function(matriceW,vecRef,f)
{
	index = which(is.na(matriceW))
	indexVal = which(!is.na(matriceW))

	if (length(index)>1) {
		print("OT des NAs")
		print(length(matriceW))
		vecRef = vecRef[-index]
		matriceW2 = matriceW[-index]

		result = lowess(vecRef,matriceW2,f=f)
		temp = result$y

		sortie = rep(NA,length(matriceW))
		sortie[indexVal]=temp
		print(length(sortie))
	}

	else{
		result = lowess(vecRef,matriceW,f=f)
		sortie = result$y
	}
	sortie
}

