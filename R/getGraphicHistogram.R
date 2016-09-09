##' @title Get Histogram of Residuals absolute
##' @description this function displays/saves a histogram graph illustrating the frequency of waste in classes
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param estatisticas data.frame containing field 'residuo'
##' @param save If you want to save enter the directory as a string
##' @param ... only for compatibility with other functions
##' @import grDevices
##' @export
getGraphicHistogram <- function(titulo="residuos", nome ="observadoXestimado", estatisticas, save=NULL, ...){
  if (!is.null(save)){
    postscript(paste0(save, nome, "Histogram.postscript"))
  }
  hist(estatisticas$residuo, xlab=iconv("Residuos"), breaks=100, main=iconv("Histograma de residuos"))
  if (!is.null(save)){
    dev.off()
  }
}
