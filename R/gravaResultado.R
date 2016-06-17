#' @title Records doc result
#' @description this function records the result in a docx file
#' @param resultado table with the results of statistical functions available in avaliaAjuste function
#' @param arquivo the name that you want to file
#' @param modelo Rdata file
#' @param template "character" value, it represents the filename of the docx file used as a template.
#' @import stringr
#' @importFrom "ReporteRs" "vanilla.table" "docx" "setZebraStyle" "addFlexTable" "writeDoc"
#' @importFrom "utils" "read.table" "write.table"
#' @import sqldf
#' @export
gravaResultado <- function(resultado, arquivo, modelo, template = "template.docx") {

  if (!file.exists(template)) {
    stop("Arquivo de template nao existe")
  }

  if (file.exists(arquivo)) {
    dfResultadoGeral = read.table(file=arquivo, sep=";", header=T)
    names(resultado) = names(dfResultadoGeral)
    dfResultadoGeral = rbind(dfResultadoGeral, resultado)
  } else {
    dfResultadoGeral = rbind(resultado)
  }

  dfResultadoGeral = sqldf("SELECT * from dfResultadoGeral order by rmse ASC, bias ASC")

  write.table(dfResultadoGeral, file=arquivo, sep=";", row.names=F)

  gravaDocResultado(dfResultado = dfResultadoGeral, arquivo = arquivo, template = template)

  save.image(paste0(modelo, ".RData"))
}


