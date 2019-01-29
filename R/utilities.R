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
#'   a filename element, and potentially a class element.
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
  if (!is.null(options_out$class)){
    class(df) <- unique(c(options_out$class, class(df)))
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

#' @title Today's date (potentially with time)
#'
#' @description Provide the current date (and optionally time) of the system.
#'
#' @param time \code{logical} indicator of whether the output should include
#'   a timestamp as well as date.
#'
#' @return Today's date, as a \code{Date} or date and time as a 
#'   \code{POSIXct}.
#'
#' @export
#'
today <- function(time = FALSE){
  if (time){
    Sys.time()
  } else{
    Sys.Date()
  }
}

#' @title Set the class(es) of an object
#'
#' @description For inclusion in a simple pipeline, set the class of an object
#'   and return it. 
#'
#' @param x The object to get the \code{class}(es).
#'
#' @param class The \code{class}(es) to apply to \code{x}.
#'
#' @return \code{x} with class(es) of \code{class}.
#'
#' @export
#'
classy <- function(x, class = NULL){
  if (!is.character(class)){
    stop("`class` must be a character vector")
  }
  class(x) <- class
  x
}

#' @title Remove any specific incomplete entries as noted by an NA
#'
#' @description Remove any incomplete entries in a table, as determined by
#'   the presence of an \code{NA} entry in a specific column 
#'   (\code{col_to_check}).
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param col_to_check A single \code{character} value of the column to use
#'   to remove incomplete entries. 
#'
#' @return df without any incomplete entries. 
#'
#' @export
#'
remove_incompletes <- function(df, col_to_check){
  if (!("data.frame" %in% class(df))){
    stop("`df` not a data.frame class object")
  }
  if (length(col_to_check) > 1){
    stop("`col_to_check` currently only able to length 1")
  }
  if (!("character" %in% class(col_to_check))){
    stop("`col_to_check` not of class character")
  }
  incompletes <- which(is.na(df[ , col_to_check]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}
