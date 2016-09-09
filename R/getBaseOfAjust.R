##' @title get database Of Ajust
##' @description this function returns the database used in the setting
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @return will be returned a string which is the database of ajust
##' @export
getBaseOfAjust <- function(ajuste){
  if(!(FALSE %in% (c("m", "data") %in% names(ajuste))))
    return(get(toString(ajuste$data), envir =  ajuste$m$getEnv()))
  if("terms" %in% names(ajuste)){
    s = gsub(")",  "", capture.output(ajuste$call))
    s = gsub("\\s", "", gsub("^.*\\(", "", s, perl = T), perl = T)
    s =str_split(s, pattern = ",")[[1]]
    for(i in 1:length(s))
      if(grepl("data", str_split(s[[i]], "=")[[1]][1]))
        return(get( str_split(s[[i]], "=")[[1]][2], envir = attributes(ajuste$terms)$.Environment))
  }
  stop(paste0("nao foi possivel obter a base para ajuuste do tipo ", class(ajuste)))
}
