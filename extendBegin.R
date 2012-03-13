extendBegin<-function(begin,sensGraph){
	
	test<-FALSE
	sens<-sensGraph[begin]
	while(test == FALSE){
		indiceValle<-begin -1
		nbegin<-max(which(sens != sensGraph[1:indiceValle]))
		if(begin == 51){ 
			test<-TRUE
		}else{
		
			tmpbegin<-nbegin-50
			test<- sum(sensGraph[tmpbegin:nbegin]) < -10
			begin<-nbegin-1
		}
		message<-paste(test," ",begin,sep="")
		print(message)
	}
	nbegin<-nbegin+1
	return(nbegin)



}
