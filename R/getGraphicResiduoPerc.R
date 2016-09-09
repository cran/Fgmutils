##' @title Get Graphic Residuals percent
##' @description this function displays/saves a graph illustrating the distribution scatter.smooth of residues
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param strVariavelXResiduo list containing variable for compare with residuals
##' @param estatisticas data.frame containing field 'residuoPERCENTUAL'
##' @param save If you want to save enter the directory as a string
##' @param labsX label x
##' @param labsy label y
##' @param ... only for compatibility with other functions
##' @import grDevices
##' @export
getGraphicResiduoPerc <- function(titulo="residuo percentual", nome ="observadoXestimado", strVariavelXResiduo=NULL, estatisticas, save=NULL, labsX = "observacao",  labsy = "residuos", ...){
  if (!is.null(save)){
    postscript(paste0(save, nome, "ResiduoPerc.postscript"))
  }
  if (is.null(strVariavelXResiduo)) strVariavelXResiduo = rownames(estatisticas)
  scatter.smooth(strVariavelXResiduo, estatisticas$residuoPERCENTUAL, col="black", xlab=labsX, ylab=labsy, main=titulo, pch=18)
  abline(0,0)
  if (!is.null(save)){
    dev.off()
  }
}
