##' @title Co variance Estatistics
##' @description this function returns a data.frame containing fields cv
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param dfEstatisticas a data.frame
##' @param baseDoAjuste  data.frame optional
##' @param formulaDoAjuste formula used in ajust
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with cv
##' @export
estatisticasCV <- function(observado, estimado, ajuste=NULL, dfEstatisticas = NULL, baseDoAjuste=NULL, formulaDoAjuste = NULL, ...){
  if (!is.null(ajuste))
    baseDoAjuste = getBaseOfAjust(ajuste = ajuste)
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  dfEstatisticas$cv = syx(observado, estimado, n=nrow(baseDoAjuste), p=length(getParametrosOfModel(ajuste, baseDoAjuste, formulaDoAjuste)))
  return(dfEstatisticas)
}
