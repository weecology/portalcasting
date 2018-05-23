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