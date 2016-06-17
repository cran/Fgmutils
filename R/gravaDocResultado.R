#' @title Records doc result
#' @description this function records the result in a docx file
#' @param dfResultado a dataset to record in doc
#' @param arquivo the name that you want to file
#' @param template "character" value, it represents the filename of the docx file used as a template.
#' @import stringr
#' @importFrom "ReporteRs" "vanilla.table" "docx" "setZebraStyle" "addFlexTable" "writeDoc"
#' @export
gravaDocResultado <- function(dfResultado, arquivo, template) {
  doc = docx(template = template)
  tabela = vanilla.table(format(dfResultado, digits=3)) %>% setZebraStyle(odd = "#E0E0E0", even = "#FFFFFF" )
  doc = addFlexTable(doc, tabela)
  writeDoc(doc, file =  str_replace(arquivo, ".csv", ".docx"))
}
