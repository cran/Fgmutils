##' @title save function with Model
##' @description save function with Model of type criaModeloGenerico or criaModeloExclusivo
##' @param modelo function with Model the save
##' @param diretorio directory to save the file, if not informed saved in the work directory
##' @import utils
##' @export
salvaModelo <- function(modelo, diretorio = ""){
  if(!is.null(diretorio)){
    if (! dir.exists(diretorio)) dir.create(diretorio, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    param = modelo()
    nome = param[1]
    type = " modelo exclusivo.txt"
    if (length(param) > 2)
      type = " modelo generico.txt"
    capture.output(modelo, file = paste0(diretorio, nome, type))
  }
}
