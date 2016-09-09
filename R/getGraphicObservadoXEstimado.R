##' @title Get Graphic Observed X Estimated
##' @description this function display/save a graphic scatter.smooth illustrating the difference between the observed and estimated
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param showTestF draw results of test F in graphic?
##' @param save If you want to save enter the directory as a string
##' @param labsX label x
##' @param labsy label y
##' @param ... only for compatibility with other functions
##' @import grDevices
##' @export
getGraphicObservadoXEstimado <- function(titulo="observadoXestimado", nome ="observadoXestimado", observado, estimado, showTestF = TRUE, save=NULL, labsX = "observado",  labsy = "estimado", ...){
  if (!is.null(save)){
    postscript(paste0(save, nome, "ObservadoXEstimado.postscript"))
  }
  scatter.smooth(observado, estimado, col="black", xlab=labsX, ylab=labsy, main=titulo, pch=18)
  abline(0,1)
  if (showTestF == TRUE)
  {
    coeficientes = lm(estimado ~ observado)
    mtext("Test F:", side=3, line=0.7)
    mtext(paste0("B0 = ", round(coeficientes$coefficients[1], digits = 3), " : B1 = ", round(coeficientes$coefficients[2], digits = 3)), side=3, line=0.0)
  }
  if (!is.null(save)){
    dev.off()
  }
}
