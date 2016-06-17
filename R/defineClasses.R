#' @title define Classes
#' @description creates a list with the class interval of a frequency distribution
#' @param limiteMin the lowest list number
#' @param limiteMax the largest number in the list
#' @param amplitude List amplitude
#' @param decrescente order by true decreasing , false increasing
#' @export
defineClasses <- function (limiteMin, limiteMax, amplitude, decrescente = TRUE) {

  nroClasses = ceiling((limiteMax-limiteMin)/amplitude)

  classes = list()
  i = 1
  inicial = floor(limiteMin)
  while (inicial<ceiling(limiteMax)) {
    if (decrescente) {
      classes[[i]] = c(nroClasses,c(inicial,((inicial+(inicial+amplitude))/2), inicial+amplitude))
    } else {
      classes[[i]] = c(i,c(inicial,((inicial+(inicial+amplitude))/2), inicial+amplitude))
    }

    inicial = inicial + amplitude
    i = i+1
    nroClasses = nroClasses - 1
  }
  return (classes)
}
