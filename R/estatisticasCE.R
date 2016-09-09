##' @title CE Estatistics
##' @description this function returns a data.frame containing fields
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with CE
##' @export
estatisticasCE <- function(observado, estimado, dfEstatisticas = NULL, ...){
  require(Fgmutils)
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$ce = ce(observados = observado, estimados = estimado)
  return(dfEstatisticas)
}
