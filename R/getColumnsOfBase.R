##' @title get Columns Of Base present in the string
##' @description this function returns the columns of a base whose names are present in the string strColumns
##' @param base data.frame
##' @param strColumns string containing name fields of the base
##' @return will be returned list with fields whose name are present in the string
##' @export
getColumnsOfBase <- function(base, strColumns){
  colunas = vector()
  nomes = names(base)
  j=0
  if( 1 <= length(names(base)))
    for(i in 1: length(names(base)))
      if (grepl(nomes[[i]], strColumns) > 0)
        colunas[[(j=j+1)]] = nomes[[i]]
  return(colunas)
}
