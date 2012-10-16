GOgraph<-function(annotation, clusterName, prefix,bestNode=5){
#--Fonction
#--Auteur
#--Description

#--Arguments
geneNames<-names(annotation)
geneList<-factor(as.integer(geneNames %in% clusterName))
names(geneList)<-geneNames
ont<-c("BP","MF","CC")

for (i in 1:length(ont)){
	GOcluster<-new("topGOdata",ontology=ont[i], allGenes = geneList, annot = annFUN.gene2GO, gene2GO = probeGOfiltre, nodeSize=3)
	resultF<-runTest(GOcluster, algorithm="classic", statistic="fisher")

	printGraph(GOcluster, resultF, firstSigNodes=bestNode, fn.prefix=paste(prefix,"_",ont[i],sep=""), useInfo="all",pdfSW=TRUE)

	resultTab<-GenTable(GOcluster, classicFisher= resultF, orderBy = "classicFisher", ranksOf = "classicFisher", topNodes = bestNode,numChar=200)

	write.table(resultTab, file=paste(prefix,"_",ont[i],".txt",sep=""),row.names= FALSE, sep="\t", quote=FALSE)
}

#----Check Argument


}


