selectUponPlot<-function(x,y,name="graph.png"){

	plot(x,y,log='xy', pch=19)
	print("tracer deux points pour definir une droite les points au dessus de la droite et entre les bords seront selectionné. Clik droit une fois fini")
	coords<-locator(type="l",col=1)
	h<-lm(coords$y ~ coords$x)
	b<-h$coefficients[1]
	a<-h$coefficients[2]
	tmp<-which( x > coords$x[1] & x < coords$x[2] & y > a*x + b )
	indexcol<-replace(indexcol,tmp,2)
	selectProbe<-y[tmp]
	namesSelectProbe<-names(x)[tmp]
	names(selectProbe) = namesSelectProbe

#	points(x[tmp],y[tmp], pch=19,col=2)
	plot(x,y,log='xy', pch=19,col=indexcol)

	return(selectProbe)


}
