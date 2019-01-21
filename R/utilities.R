#' @title Save data out to a file and return it
#'
#' @description Save inputted data out to a data file if requested via an 
#'   options list (request is handled by the \code{save} element and the file
#'   name to use is handled by the \code{filename} element) and return it 
#'   to the console.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param options_out an options list that includes a save element and 
#'   a filename element
#'
#' @return df (as input)
#'
#' @export
#'
dataout <- function(df, options_out = moons_options()){
  if (! "data.frame" %in% class(df)){
    stop("`df` not a data.frame")
  }
  if (!is.null(options_out$save)){
    if (options_out$save){
      file_path(options_out$tree, paste0("data/", options_out$filename)) %>%
      write.csv(df, ., row.names = FALSE)
    }
  }
  df
}

#' @title Save data out to a csv, appending the file if it already exists
#'
#' @description Appending a \code{.csv} without re-writing the header of the
#'   file. If the doesn't exist, it will be created.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param filename \code{character} filename of existing \code{.csv} to be 
#'   appended.
#'
#' @export
#'
append_csv <- function(df, filename){
  if (length(filename) > 1){
    stop("`filename` can only be of length = 1")
  }
  if (!is.character(filename)){
    stop("`filename` not a character")
  }
  if (! "data.frame" %in% class(df)){
    stop("`df` not a data.frame")
  }
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
}

#' @title Zero-abundance forecast
#'
#' @description Create a 0-abundance forecast for fill-in usage when a model 
#'   fails or there is no non-0 historical abundance.
#'
#' @param nfcnm \code{integer} number of forecast newmoons.
#'
#' @param pred_name \code{character} name for the predictor column (to match
#'   variable model output names).
#'
#' @return Two-element \code{list} of means and interval values for a 
#'   0-abundance forecast to be used as a filler when a model fails or there 
#'   is no non-0 historical abundance.
#'
#' @export
#'
fcast0 <- function(nfcnm, pred_name = "pred"){
  if (length(nfcnm) > 1){
    stop("`nfcnm` can only be of length = 1")
  }
  if (nfcnm %% 1 != 0){
    stop("`nfcnm` not an integer")
  }
  if (!is.character(pred_name)){
    stop("`pred_name` not a character")
  }
  if (length(pred_name) > 1){
    stop("`pred_name` can only be of length = 1")
  }
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list(mean_0, interval = int_0)
  names(out)[1] <- pred_name
  out
}

#' @title Today's date
#'
#' @description Provide the current date of the system.
#'
#' @return Today's date, as a \code{Date}.
#'
#' @export
#'
today <- function(){
  Sys.Date()
}

