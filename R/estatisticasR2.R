##' @title R2 Estatistics for linear models
##' @description this function returns a data.frame containing fields r2
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param dfEstatisticas a data.frame
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param intercepto intercepts?
##' @param formulaDoAjuste formula used in ajust
##' @param baseDoAjuste  data.frame optional
##' @param ... only for compatibility with other functions
##' @return will be returned data.frame with r2
##' @export
estatisticasR2 <- function(observado, estimado, dfEstatisticas = NULL, ajuste = NULL, intercepto = TRUE, formulaDoAjuste = NULL, baseDoAjuste=NULL, ...){
  if(is.null(dfEstatisticas))
    dfEstatisticas = data.frame(observado = observado, estimado = estimado)
  if (intercepto) {
    dfEstatisticas$r2 = R21a(observados = observado, estimados = estimado, k=length(getParametrosOfModel(ajuste, baseDoAjuste, formulaDoAjuste)))
  } else {
    dfEstatisticas$r2 = R29a(observados = observado, estimados = estimado, k=length(getParametrosOfModel(ajuste, baseDoAjuste, formulaDoAjuste)))
  }
  return(dfEstatisticas)
}
