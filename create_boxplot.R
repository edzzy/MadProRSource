`create_boxplot` <-
function (data,output_name)
##fonction pour créer des boites à moustaches
{
  bottomarg = nchar(max(colnames(data))) #nombre de ligne pour la marge du bas
  jpeg(filename = output_name, width = 1300, height = 900, quality = 100, bg = "white", res = NA)
  par(mar=c(bottomarg + 5,5,3,3))
  boxplot(log(as.data.frame(data)), col=rainbow(n=20), las="2",cex.lab=1.5,cex.axis=1.5)
  dev.off()
}

