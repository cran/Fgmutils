#' @title Check de type of Column
#' @description this function returns the type of a column of a dataFrame, if it is numeric or character.
#' @param coluna column of dataframe
#' @examples
#' ID_REGIAO <- c(1936,1936,1941,1934)
#' CD_PLANTIO <- c("2526","2526","2527","2527")
#' test <- data.frame(ID_REGIAO,CD_PLANTIO)
#' verificaTipoColuna(test$ID_REGIAO)
#' @export
verificaTipoColuna <- function(coluna){
  if(typeof(coluna) == "character")
  {

    return ("as.character()");
  }
  return ("as.numeric()");

}
