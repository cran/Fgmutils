##' @title Estatistics
##' @description this function returns a data.frame containing fields observado and estimado
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with observado and estimado fields add
##' @export
estatisticas <- function(observado, estimado, dfEstatisticas = NULL, ...){
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  return(dfEstatisticas)
}
