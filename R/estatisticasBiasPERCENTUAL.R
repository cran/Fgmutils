##' @title percent BIAS Estatistics
##' @description this function returns a data.frame containing fields biasPERCENTUAL
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame with field bias
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with biasPERCENTUAL
##' @export
estatisticasBiasPERCENTUAL <- function(observado, estimado, dfEstatisticas, ...){
  require(Fgmutils)
  if(!("bias" %in% names(dfEstatisticas)))
    stop("ERROR: FIRST CALC BIAS")
  dfEstatisticas$biasPERCENTUAL = calculaPerc(dfEstatisticas$bias, observados = observado)
  return(dfEstatisticas)
}
