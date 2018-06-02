#' Appending a csv without re-writing the header.
#' @param df data table to be written out
#' @param filename filename of existing csv to be appended
#' @return Nothing
#' @export
#'
append_csv <- function(df, filename){
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
}

#' Create a 0-abundance forecast
#' @param nfcnm number of forecast newmoons
#' @return list of mean and interval values for a 0-abundance filler forecast
#' @export
#'
fcast0 <- function(nfcnm){
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list("pred" = mean_0, "interval" = int_0)
  return(out)
}