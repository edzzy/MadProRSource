extendEnd<-function(end,sensGraph){

	test<-FALSE
	sens<-sensGraph[end]


	while(test == FALSE){
		indiceDescente<-end+1
		nend<-min(which(sensGraph[end] != sensGraph[indiceDescente:length(sensGraph)]))	
		nend<-length(sensGraph[1:end]) +nend
		tmpend<- nend + 50
		test<-sum(sensGraph[nend:tmpbegin]) < -10
		end<-nend+1
		}	

	return(nend)


}
