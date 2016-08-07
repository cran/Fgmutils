##' @title Get List of DAP Classes
##' @description this function return a list of data.frame where each contains a number of dap classes according to reported basis
##' @param base the data.frame  containing fields limiteMin, limiteMax of parcela and idadearred
##' @param amplitude it is amplitude of dap class
##' @param verbose use TRUE to show status of process
##' @return list of data.frame
##' @export
getClasses <- function(base, amplitude, verbose=FALSE){
  if (verbose) print("getting class list")
  classes = list();
  linhas = nrow(base)
  for (i in 1:linhas){
    min = base[i, "limiteMin"]
    max = base[i, "limiteMax"]
    classes[[i]] = defineClasses(limiteMin = min, limiteMax = max, amplitude,  getDataFrame = TRUE, verbose = verbose)
    if (verbose) print(paste0(i, " class for ", linhas, " [ ", min, " to ", max, " ]"))
  }
  return (classes)
}
