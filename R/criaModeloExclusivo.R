##' @title Create Exclusive Model for a database
##' @description this function returns a unique model is variable receive each mapeda variable ex .: criaModeloExclusivo (modeloCamposLeite, c ("age1", "age2", "bai1", "s"))
##' @param modeloGenerico model of pattern criaModeloGenerico
##' @param variaveis list of name fields (strings) in database and model, the order of variables matter
##' @param palpite string containing start values of function of regression
##' @return will be returned a function with exclusive model
##' @export
criaModeloExclusivo <- function(modeloGenerico, variaveis, palpite=NULL){ 
  var = NULL
  eval(parse(text = "var = modeloGenerico()"))
  if (is.null(var)) stop("modelo generico invalido")
  if (length(var) <= 2 || (length(var) - 2) != length(variaveis)) stop("numero de parametros invalido")
  str = paste0(var[3] , " = \"", variaveis[1], "\"")
  if(2 <= length(variaveis))
    for(i in 2:length(variaveis))
      str = paste0(str, ", ", var[2+i], " = \"", variaveis[i],"\"")
  if(!is.null(palpite))
    str = paste0(", palpite=c(", palpite, ")")
  modeloExclusivo = NULL
  eval(parse(text = paste0("modeloExclusivo = modeloGenerico(", str, ")")))
  return (modeloExclusivo)
}
