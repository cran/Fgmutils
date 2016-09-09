##' @title percent RMSE Estatistics
##' @description this function returns a data.frame containing fields rmsePERCENTUAL
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame containing field rmse
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with rmse PERCENTUAL calc
##' @export
estatisticasRmsePERCENTUAL <- function(observado, estimado, dfEstatisticas, ...){
  if(!("rmse" %in% names(dfEstatisticas)))
    stop("ERROR: FIRST CALC RMSE")
  dfEstatisticas$rmsePERCENTUAL = calculaPerc(dfEstatisticas$rmse, observados = observado)
  return(dfEstatisticas)
}
