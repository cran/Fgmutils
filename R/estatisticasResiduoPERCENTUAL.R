##' @title Residuals Estatistics
##' @description this function returns a data.frame containing field residuoPERCENTUAL
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame containing field residuo
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with percent Residuals field
##' @export
estatisticasResiduoPERCENTUAL <- function(observado, estimado, dfEstatisticas = NULL, ...){
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$residuoPERCENTUAL = residuoPerc(estimados = estimado, observados = observado)
  return(dfEstatisticas)
}
