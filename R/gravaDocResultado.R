## Copyright (C) 2016  Clayton Vieira Fraga Filho
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##  
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

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
