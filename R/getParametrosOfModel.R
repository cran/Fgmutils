##' @title get Parametros Of Model
##' @description this function retona columns the base of the parameter or setting present in the model
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param base optional data.frame whose fields name is present in formula
##' @param formula string containing name fields of the base
##' @return will be returned list of columns used in ajust or in formula
##' @export
getParametrosOfModel <- function(ajuste, base=NULL, formula = NULL){
  if (is.null(ajuste)){
    if(is.null(base) || is.null(formula))
      stop("se ajuste eh null base e formula devem ser informados e vice versa")
    return(getColumnsOfBase(base=base, strColumns = formula))
  }
  return(getColumnsOfAjust(ajuste = ajuste))
}
