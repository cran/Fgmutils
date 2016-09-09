##' @title Percent Co variance Estatistics
##' @description this function returns a data.frame containing fields cvPERCENTUAL
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame with cv field
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with cvPERCENTUAL
##' @export
estatisticasCvPERCENTUAL <- function(observado, estimado, dfEstatisticas, ...){
  if(!("cv" %in% names(dfEstatisticas)))
    stop("ERROR: FIRST CALC CV")
  dfEstatisticas$cvPERCENTUAL = syxPerc(dfEstatisticas$cv, observados = observado)
  return(dfEstatisticas)
}
