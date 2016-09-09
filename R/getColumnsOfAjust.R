##' @title get Columns used in Ajust
##' @description this function returns an array with the column names that are on the model and reported basis or basis used in ajust
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param dfDados data.frame optional
##' @param excludeY1andY2 delete Y1 and Y2 fields? del formula(y1~y2...)
##' @return will be returned list of columns used in ajust
##' @export
getColumnsOfAjust <- function(ajuste, dfDados=NULL, excludeY1andY2 = T){
  if (is.null(dfDados))
    dfDados = getBaseOfAjust(ajuste)
  formula = getFormulaExclusivaOfAjust(ajuste)
  if(excludeY1andY2)
    formula = gsub("^.*~ \\S+", "", formula, perl = T)
  return(getColumnsOfBase(base=dfDados, strColumns = formula))
}
