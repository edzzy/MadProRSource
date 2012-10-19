library(RUnit)
test.arrayCDTimage.R<-function(){}
test.calculeNorm.R<-function(){}
test.clusterbyPair.R<-function(){}
test.create_boxplot.R<-function(){}
test.create_correlation_median.R<-function(){}
test.createFactor4matrix2png.R<-function(){}
test.create_level_matrix.R<-function(){}
test.createMap4matrix2png.R<-function(){}
test.createMatrix2png.R<-function(){}
test.createNamesArray.R<-function(){}
test.create_pathway.R<-function(){}
test.create_stats_des.R<-function(){}
test.CV.R<-function(){}
test.delete.control.in.matrix.R<-function(){}
test.detectCuster.R<-function(){}
test.doAllImageArray.R<-function(){}
test.extendBegin.R<-function(){}
test.extendEnd.R<-function(){}
test.FC.R<-function(){

	source("FC.R")

	checkEquals(FC(c(2,1)),2)
	checkEquals(FC(c(1,2)),-2)
	checkEquals(FC(c(2,2)),1)
	checkException(FC(c(1)),silent=TRUE)
	checkException(FC(c(0,0)),silent=TRUE)
	checkException(FC(c(1,2,3)), silent=TRUE)
	checkException(FC(c("A","B")), silent=TRUE)
	checkException(FC(c(-1,-2)), silent=TRUE)
	checkException(FC(c(1,-2)), silent=TRUE)
	checkException(FC(c(-1,2)), silent=TRUE)
}
test.filtrage_non_exprimes.R<-function(){}
test.fusionCluster.R<-function(){}
test.getUpDown.R<-function(){}
test.graphClustPval.R<-function(){
	source("graphClustPval.R")

	checkEquals(graphClustPval(c(1,2,0.001)),-3)
	checkEquals(graphClustPval(c(2,1,0.001)),3)
	checkEquals(graphClustPval(c(2,1,NA)),NA)
	checkEquals(graphClustPval(c(2,2,0.001)),-3)
}
test.graphMmobile.R<-function(){}
test.invariant.R<-function(){}
test.LOWESS.R<-function(){}
test.madPro.R<-function(){}
test.makeRatio.R<-function(){}
test.meanByFact.R<-function(){}
test.my.lowess.R<-function(){}
test.normQuantile.R<-function(){}
test.order2cdtMatrix.R<-function(){}
test.order_by_cdt.R<-function(){}
test.pair.Rows.t.test.R<-function(){}
test.read_Agilent.R<-function(){}
test.read.cdt.R<-function(){}
test.read.matrixArray.R<-function(){}
test.recentrage.R<-function(){}
test.rmArrayByNames.R<-function(){}
test.sampleMatrix.R<-function(){}
test.selectUponPlot.R<-function(){}
test.testLo<-function(){}
test.test.R<-function(){}
test.tex_clusterImage.R<-function(){}
test.tex_Corre.R<-function(){}
test.tex_filtrage.R<-function(){}
test.tex_genDiff.R<-function(){}
test.tex_importData.R<-function(){}
test.tex_Norm.R<-function(){}
test.tex_question.R<-function(){}
test.tex_StatDesc.R<-function(){}
test.traceAvantApres.R<-function(){}
test.traceGraph.R<-function(){}
test.UD.R<-function(){}
test.writeMatrice.R<-function(){}
