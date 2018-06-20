#' @title Save data out to a file and return it
#'
#' @description Save data out to a data file if requested (request is handled
#'   by the save element in the data_options list and the file name is handled
#'   by the filename element) and return it to the console
#'
#' @param data data table to be written out
#'
#' @param tree directory tree
#'
#' @param data_options data_options list that includes a save element and 
#'   a filename element
#'
#' @return data (as input)
#'
#' @export
#'
dataout <- function(data, tree = dirtree(), data_options = rodents_options()){

  if (!is.null(data_options$save)){
    if (data_options$save){
      path <- file_path(tree = tree, paste0("data/", data_options$filename))
      write.csv(data, path, row.names = FALSE)
    }
  }
  return(data)
}

#' @title Save data out to a csv, appending if existing
#'
#' @description Appending a csv without re-writing the header. If file doesn't
#'   exist, it will be created.
#'
#' @param df data table to be written out
#'
#' @param filename filename of existing csv to be appended
#'
#' @return Nothing
#'
#' @export
#'
append_csv <- function(df, filename){
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
}

#' @title Zero-abundance forecast
#'
#' @description Create a 0-abundance forecast for fill-in usage
#'
#' @param nfcnm number of forecast newmoons
#'
#' @param pred_name name for the predictor column (to match variable model
#'   output names)
#'
#' @return list of mean and interval values for a 0-abundance filler forecast
#'
#' @export
#'
fcast0 <- function(nfcnm, pred_name = "pred"){
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list(mean_0, "interval" = int_0)
  names(out)[1] <- pred_name
  return(out)
}

#' @title Today's date
#'
#' @description name-shortening function
#'
#' @return today's date
#'
#' @export
#'
today <- function(){
  Sys.Date()
}

