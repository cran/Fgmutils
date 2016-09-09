##' @title Correlacion Estatistics
##' @description this function returns a data.frame containing fields corr
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with corr field
##' @export
estatisticasCORR <- function(observado, estimado, dfEstatisticas = NULL, ...){
  #require(Fgmutils)
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$corr = cor(observado,  estimado)
  return(dfEstatisticas)
}
