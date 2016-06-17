#' @title add column
#' @description  take a data-frame and a vector and combine by columns, respectively.
#' @param dataf dataframe
#' @param vec vector
#' @param namevec the names of the columns of vector
#' @return dataf dataframe combined with the vector
#' @export
add.col <- function(dataf, vec, namevec) {
  options(warn=-1)
  if (nrow(dataf) < length(vec) ) {
    dataf <-  rbind(dataf, matrix(NA, length(vec)-nrow(dataf), ncol(dataf),	dimnames=list( NULL, names(dataf) ) ) )
  }
  length(vec) <- nrow(dataf) # pads with NA's
  dataf[, namevec] <- vec; # names new col properly
  options(warn=1)
  return(dataf)
}
