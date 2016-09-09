##' @title get Formula Exclusive Of Ajust
##' @description this function returns the formula of the model used in ajust
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @return will be returned a string which is the formula of ajust
##' @export
getFormulaExclusivaOfAjust <- function(ajuste){
  if(!(FALSE %in% (c("m", "data") %in% names(ajuste)))){
    saida = capture.output(ajuste$m$formula())
    formula = saida[[1]]
    if(2 <= (length(saida)-1))
      for(i in 2:(length(saida)-1)){
        if (grepl("<environment:", saida[[i]]) > 0)
          break
        formula = paste0(formula, saida[[i]])
      }
    return(formula)
  }
  if("terms" %in% names(ajuste)){
    saida = capture.output(formula(ajuste$terms))
    formula = saida[[1]]
    if(2 <= (length(saida)-1))
      for(i in 2:(length(saida)-1)){
        if (grepl("<environment:", saida[[i]]) > 0)
          break
        formula = paste0(formula, saida[[i]])
      }
    return(formula)
  }
  stop(paste0("nao foi possivel obter a formula para ajuuste do tipo ", class(ajuste)))
}
