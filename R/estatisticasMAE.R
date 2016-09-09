##' @title MAE Estatistics
##' @description this function returns a data.frame containing fields mae
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with mae
##' @export
estatisticasMAE <- function(observado, estimado, dfEstatisticas = NULL, ...){
  require(Fgmutils)
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$mae  =  mae(observados = observado, estimados = estimado)
  return(dfEstatisticas)
}
