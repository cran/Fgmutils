##' @title Percent Correlacion Estatistics
##' @description this function returns a data.frame containing fields corr_PERCENTUAL
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame with corr field
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with corr_PERCENTUAL field
##' @export
estatisticasCorrPERCENTUAL <- function(observado, estimado, dfEstatisticas, ...){
  require(Fgmutils)
  if(!("corr" %in% names(dfEstatisticas)))
    stop("ERROR: FIRST CALC CORR")
  dfEstatisticas$corr_PERCENTUAL = calculaPerc(dfEstatisticas$corr, observados = observado)
  return(dfEstatisticas)
}
