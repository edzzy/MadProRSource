tex_question<-function(vectFile,texFile){
	
	for(i  in 1:length(vectFile)){
		text<-paste("\\input{",vectFile[i],"}\n",sep="")
		if(i == 1){
			cat(text,file=texFile,append=FALSE)
		}else{
			cat(text,file=texFile,append=TRUE)
		}
	}
	
}