##' @title Field Converts To Character
##' @description converts a column of a dataframe to String
##' @param nomeCampo the column name you want to convert
##' @param base the column having dataFrame, that you want to convert to String
##' @return base dataFrame with a column converted to String
##' @examples
##' DATA_MEDICAO <- c(02/2009,02/2010,02/2011,02/2011)
##' CD_PARCELA <- c(6947,6947,6947,6947)
##' test <- data.frame(DATA_MEDICAO,CD_PARCELA)
##' converteCampoParaCharacter("DATA_MEDICAO",test)
##' @export
converteCampoParaCharacter <- function (nomeCampo, base){
  eval(parse(text=paste0("base$",nomeCampo," = as.character(base$", nomeCampo, ")")))
  return (base);
}
