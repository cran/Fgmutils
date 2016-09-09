##' @title Residuals Estatistics
##' @description this function returns a data.frame containing field residuo
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with Residuals field
##' @export
estatisticasResiduos <- function(observado, estimado, dfEstatisticas = NULL, ...){
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$residuo = observado - estimado
  return(dfEstatisticas)
}
