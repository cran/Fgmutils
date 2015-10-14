##' @title Get Year Measurement
##' @description using DATA_MEDICAO column in the form MM/YYYY creates a new column with the name "ANO_MEDICAO" in YYYY format
##' @param dataFrame that has the column DATA_MEDICAO,CD_PARCELA
##' @return dataFrame dataframe that has columns COD_PARCELA, DATA_MEDICAO, ANO_MEDICAO
##' @import stringr
##' @import plyr
##' @import siar
##' @examples
##' DATA_MEDICAO <- c("02/2009","02/2010","02/11","02/2011")
##' CD_PARCELA <- c(6947,6947,6947,6947)
##' test <- data.frame(DATA_MEDICAO,CD_PARCELA)
##' getAnoMedicao(test)
##' @export
getAnoMedicao <- function (dataFrame) {
  #Get the name of dataFrame.
  nomedataFrame = toString(substitute(dataFrame))
  sql ="SELECT distinct CD_PARCELA, DATA_MEDICAO from dataFrame group by CD_PARCELA, DATA_MEDICAO"
  sql = gsub("dataFrame", nomedataFrame, sql)

  dataFrame = sqldf(sql)
  df <- ldply(str_split(dataFrame$DATA_MEDICAO,  "/"))[,2]
  dataFrame = cbind(dataFrame, df)
  #id_col = which(names(dataFrame) == "df") # Get the identifier column of a dataframe.
  names(dataFrame)[3] = "ANO_MEDICAO"
  dataFrame$ANO_MEDICAO = as.numeric(str_c(dataFrame$ANO_MEDICAO))

  #to ensure it will not be concatenated with any date that already has 0 20 before.
  if(str_length(dataFrame$ANO_MEDICAO[1])==2)
  {
    dataFrame$ANO_MEDICAO = as.numeric(str_c("20", dataFrame$ANO_MEDICAO))
  }

  return (dataFrame)
}
