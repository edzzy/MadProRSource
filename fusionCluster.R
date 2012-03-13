fusionCluster<-function(begin,end){

	interXY<-0
	clusterTotal<-length(begin)
	finaleBegin<-NULL
	finaleEnd<-NULL				

	while(length(begin) != 1){		
		b1<- begin[1]
		e1<- end[1]
		b2<- begin[2]
		e2<- end[2]

		x<-seq(b1:e1) + b1 - 1
		y<-seq(b2:e2) + b2 - 1
		interXY<-intersect(x,y)  
		if(length(interXY) == 0){
			finaleBegin<-c(finaleBegin,b1)
			finaleEnd<-c(finaleEnd,e1)
			begin<-begin[-1]
			end<-end[-1]
		}else{
			rXY<-range(union(x,y))
			begin<-begin[-1]
			end<-end[-1]
			begin[1]<-rXY[1]
			end[1]<-rXY[2]

		} 
	}

	finaleBegin<-c(finaleBegin,begin)	
	finaleEnd<-c(finaleEnd,end)	

	coordClust<-cbind(finaleBegin,finaleEnd)
	print(finaleBegin)
	print(finaleEnd)
	return(coordClust)
}
