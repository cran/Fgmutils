##' @title BIAS Estatistics
##' @description this function returns a data.frame containing fields bias
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with bias
##' @export
estatisticasBIAS <- function(observado, estimado, dfEstatisticas = NULL, ...){
  require(Fgmutils)
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$bias = bias(observado,  estimado)
  return(dfEstatisticas)
}
