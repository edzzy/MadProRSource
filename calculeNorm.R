`calculeNorm` <-
function(increm, echantillon, lowessCurve, ref)
{
	result = echantillon[,increm]/lowessCurve[,increm]*ref
	return(result)
}

