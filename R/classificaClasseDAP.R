#' @title classifica Classe DAP
#' @description the center of the class that the DAP belongs.
#' @param dfClassesDAP a frequency distribution with the attributes $classe and $centro
#' @param dap integer Diameter at breast height
#' @param  getNhaClasse get NhaClasse field of dfClassesDAP, default false
#' @param  getNCLASSES get NCLASSES field of dfClassesDAP, default false
#' @examples
#' dados = defineClasses(1, 10, 2, getDataFrame = TRUE)
#' classificaClasseDAP(dados,7)
#' @export
classificaClasseDAP <- function(dfClassesDAP, dap, getNhaClasse = FALSE, getNCLASSES = FALSE) {
  linhas = nrow(dfClassesDAP)
  if(linhas == 1 && ((dap >= dfClassesDAP[1,]$linf) && (dap <= dfClassesDAP[1,]$lsup))){
    if (getNCLASSES == TRUE){
      return (dfClassesDAP[1, ]$NCLASSES)
    }
    if (getNhaClasse == TRUE){
      return (dfClassesDAP[1, ]$NhaClasse)
    }
    return(dfClassesDAP[1,]$centro)
  }
  if (linhas > 1){
    for (i in 1:linhas) {
      if (dap >= dfClassesDAP[i,]$linf && (dap < dfClassesDAP[i,]$lsup || (i == linhas && dap <= dfClassesDAP[i,]$lsup))) {
        if (getNCLASSES == TRUE){
          return (dfClassesDAP[i, ]$NCLASSES)
        }
        if (getNhaClasse == TRUE){
          return (dfClassesDAP[i, ]$NhaClasse)
        }
        return(dfClassesDAP[i,]$centro)
      }
    }
  }
  print(paste0('impossivel classificar dap ', dap, ' para ', dfClassesDAP[1,]$linf, ' A ', dfClassesDAP[linhas,]$lsup))
  return(-1)
}
