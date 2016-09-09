##' @title calculates Volume Default
##' @description this function calculates the volume based on the height and volume of literature of the equation
##' @param ht is list of height of individuals
##' @param dap is list of diameter of individuals
##' @param ... only for compatibility with other functions
##' @return will be returned a list of volume calc
##' @export
calculaVolumeDefault <- function(ht, dap, ...){
  p1 = list()
  p1$b0 = -10.1399754928663
  p1$b1 = 1.86835930704287
  p1$b2 = 1.07778665273381
  return(exp(p1$b0+p1$b1*log(dap) + p1$b2*log(ht)))
}
