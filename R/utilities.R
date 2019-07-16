#' @title Check if a URL will throw errors
#'
#' @description If the URL throws errors, return them gracefully. 
#'
#' @param url \code{character} value to evaluate.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'   quieted.
#'
#' @return Error message or \code{NULL} otherwise.
#' 
#' @export 
#'
check_url <- function(url, quiet = FALSE){
  writefun <- basicTextGatherer(.mapUnicode = .mapUnicode)$update
  opts <- curlOptions(URL = url, writefunction = writefun)
  curl <- getCurlHandle()
  out <- tryCatch(y <- curlPerform(curl = curl, .opts = opts),
                  error = function(x){as.character(x$message)})
  if(is.integer(out)){
    out <- NULL
  }
  out
}


#' @title Replace if NULL
#'
#' @description If the focal input is \code{NULL}, replace it with 
#'   alternative. 
#'
#' @param x Focal input.
#'
#' @param alt Alternative value.
#'
#' @return \code{x} if not \code{NULL}, \code{alt} otherwise.
#' 
#' @export 
#'
ifnull <- function(x = NULL, alt = NULL){
  if(is.null(x)){
    x <- alt
  }
  x
}


#' @title Optionally generate a message based on a logical input
#'
#' @description Given the input to \code{quiet}, generate the message(s) 
#'   in \code{msg} or not.
#'
#' @param msg \code{character} vector of the message(s) to generate or 
#'   \code{NULL}. If more than one element is contained in \code{msg}, they
#'   are concatenated with a newline between.
#'
#' @param quiet \code{logical} indicator controlling if the message is
#'   generated.
#'
#' @export
#'
messageq <- function(msg = NULL, quiet = FALSE){
  if (!quiet){
    msg2 <- paste(msg, collapse = "\n")
    message(msg2)
  }
}


#' @title Conform NA entries to "NA" entries
#'
#' @description Given the species abbreviation NA, when data are read in, 
#'   there can be an \code{NA} when it should be an \code{"NA"}. This function
#'   conforms the entries to be proper character values. 
#'
#' @param x Either [1] a \code{data.frame} containing \code{colname} as a 
#'   column with \code{NA}s that need to be conformed to \code{"NA"}s or [2]
#'   a vector with \code{NA}s that need to be conformed to \code{"NA"}s
#'
#' @param colname \code{character} value of the column name in \code{tab} to 
#'   conform the \code{NA}s to \code{"NA"}s.
#'
#' @return \code{x} with any \code{NA} in \code{colname} replaced with 
#'   \code{"NA"}.
#'
#' @export
#'
na_conformer <- function(x, colname = "species"){
  check_args()
  if (is.vector(x)){
    naentries <- which(is.na(x))
    x[naentries] <- "NA"
  } else if (is.data.frame(x)){
    nasppname <- which(is.na(x[ , colname]))
    if (length(nasppname) > 0){
      x[nasppname, colname] <- "NA"
    }
  } 
  x
}

#' @title Save data out to a file and return it	
#'
#' @description Save inputted data out to a data file if requested via an 
#'   options list (request is handled by the \code{save} element and the file
#'   name to use is handled by the \code{filename} element) and return it 
#'   to the console.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param control \code{list} that includes control parameters.
#'
#' @return df (as input)
#'
#' @export
#'
dataout <- function(df, tree = dirtree(), control = list()){
  cn <- names(control)
  sv <- grep("save", cn)
  fn <- grep("filename", cn)
  if (!is.null(control[[sv]]) & !is.null(control[[fn]])){
    if (control[[sv]]){
      file_paths(tree, paste0("data/", control[[fn]])) %>%
      write.csv(df, ., row.names = FALSE)
    }
  }
  if (!is.null(control$class)){
    class(df) <- unique(c(control$class, class(df)))
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
  check_args()
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
#' @param colname \code{character} name for the predictor column (to match
#'   variable model output names).
#'
#' @return Two-element \code{list} of means and interval values for a 
#'   0-abundance forecast to be used as a filler when a model fails or there 
#'   is no non-0 historical abundance.
#'
#' @export
#'
fcast0 <- function(nfcnm, colname = "pred"){
  check_args()
  mean_0 <- rep(0, nfcnm)
  int_0 <- data.frame("lower" = rep(0, nfcnm), "upper" = rep(0, nfcnm))
  out <- list(mean_0, interval = int_0)
  names(out)[1] <- colname
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
#' @examples
#' \dontrun{
#' 
#' today()
#' today(time = TRUE)
#' }
#'
#' @export
#'
today <- function(time = FALSE){
  check_args()
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
  check_args()
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
#' @param colname A single \code{character} value of the column to use
#'   to remove incomplete entries. 
#'
#' @return df without any incomplete entries. 
#'
#' @export
#'
remove_incompletes <- function(df, colname){
  check_args()
  incompletes <- which(is.na(df[ , colname]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}

#' @title Check a function's arguments' values for validity
#'
#' @description \code{check_args} checks that all of the arguments to a given
#'   function have valid values within the pipeline to avoid naming collision
#'   and improper formatting, by wrapping around \code{check_arg} for each
#'   argument. \cr \cr
#'   \code{check_arg} produces (returns) a \code{NULL} if the 
#'   argument is valid or \code{character} vector of error messages associated 
#'   with the specific situation (\code{<fun_name>(<arg_name> = arg_value)})
#'   if the argument is invalid. \cr \cr
#'   \code{check_args} should only be called within a function. \cr \cr
#'   See \code{Details} for argument-specific rules.
#'
#' @export
#'
check_args <- function(){
  fun_call <- match.call.defaults(definition = sys.function(-1), 
                                  call = sys.call(-1))
  fun_name <- toString(fun_call[[1]])
  arg_values <- fun_call[-1]
  arg_names <- names(arg_values)
  nargs <- length(arg_names)
  out <- NULL
  if(nargs > 0){
    for(i in 1:nargs){
      arg_value <- eval.parent(arg_values[[i]], 2)
      out <- c(out, check_arg(arg_names[i], arg_value, fun_name))
    }
  }
  if (!is.null(out)){
    if (length(out) > 1){
      out2 <- paste(out, collapse = "; ")
    } else{
      out2 <- out
    }
    out3 <- paste0("in ", fun_name, ": ", out2)
    stop(out3)
  }
}


check_arg <- function(arg_name, arg_value, fun_name = NULL){
  out <- NULL
  if (arg_name == "add"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`add` is not a character")
      }
    }
  }
  if (arg_name == "add_error"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`add_error` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`add_error` can only be of length = 1")
    }
  }
  if (arg_name == "add_in_window"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`add_in_window` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`add_in_window` can only be of length = 1")
    }
  }
  if (arg_name == "add_lead"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`add_lead` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`add_lead` can only be of length = 1")
    }
  }
  if (arg_name == "add_obs"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`add_obs` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`add_obs` can only be of length = 1")
    }
  }
  if (arg_name == "all"){
    if (!("list" %in% class(arg_value))){
      out <- c(out, "`all` is not a list")
    }
    if (!all(c("forecast", "aic") %in% names(arg_value))){
      out <- c(out, "`all` does not have elements named `forecast` and `aic`")
    }
  }
  if (arg_name == "all_forecasts"){
    if (!("data.frame" %in% class(arg_value))){
      out <- c(out, "`all_forecasts` is not a data frame")
    }
  }
  if (arg_name == "append_fcast_csv"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`append_fcast_csv` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`append_fcast_csv` can only be of length = 1")
    }
  }
  if (arg_name == "append_missing_to_raw"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`append_missing_to_raw` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`append_missing_to_raw` can only be of length = 1")
    }
  }
  if (arg_name == "base"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`base` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`base` can only be of length = 1")
    }
  }
  if (arg_name == "cast"){
    if (!("data.frame" %in% class(arg_value))){
      out <- c(out, "`cast` is not a data frame")
    }
    if(!cast_is_valid(arg_value)){
      out <- c(out, "`cast` is not valid")
    }
  }
  if (arg_name == "casts"){
    if (!("casts" %in% class(arg_value))){
      out <- c(out, "`casts` is not of class casts")
    }
  }
  if (arg_name == "cast_date"){
    if (!is.null(arg_value)){
      if (length(arg_value) != 1){
        out <- c(out, "`cast_date` can only be of length = 1")
      }
      cast_date2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (any(is.na(cast_date2))){
        out <- c(out, "`cast_date` is not a Date")
      }
    }
  }
  if (arg_name == "cast_dates"){
    if (!is.null(arg_value)){
      cast_dates2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (any(is.na(cast_dates2))){
        out <- c(out, "`cast_dates` is not a Date")
      }
    }
  }
  if (arg_name == "cast_to_check"){
    if (!("data.frame" %in% class(arg_value))){
      out <- c(out, "`cast_to_check` is not a data frame")
    }
  }
  if (arg_name == "cast_type"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`cast_type` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`cast_type` can only be of length = 1")
    }
    if (!(all(arg_value %in% c("forecasts", "hindcasts")))){
      out <- c(out, "`cast_type` can only be `forecasts` or `hindcasts`")
    }
  }
  if (arg_name == "class"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`class` is not a character")
      }
    }
  }
  if (arg_name == "colname"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`colname` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`colname` can only be of length = 1")
    }
  }
  if (arg_name == "confidence_level"){
    if (length(arg_value) != 1){
      out <- c(out, "`confidence_level` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`confidence_level` is not numeric")
    } else if (any(arg_value < 0.001 | arg_value > 0.999)){
      out <- c(out, "`confidence_level` is not between 0.001 and 0.999")
    }
  }
  if (arg_name == "controls"){
    if (!("list" %in% class(arg_value))){
      out <- c(out, "`controls` is not a list")
    }
    if (!all(c("forecast", "aic") %in% names(arg_value))){
      msg <- "`controls` does not have elements named `forecast` and `aic`"
      out <- c(out, msg)
    }
  }
  if (arg_name == "covariates"){
    if (!("covariates" %in% class(arg_value))){
      out <- c(out, "`covariates` is not a covariates table")
    }
  }
  if (arg_name == "covariate_source"){
    if (!is.null(arg_value)){
      valid_names <- c("current_archive", "retroactive")
      if (!is.character(arg_value)){
        out <- c(out, "`covariate_source` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`covariate_source` can only be of length = 1")
      }
      if (!(all(arg_value %in% valid_names))){
        out <- c(out, "`covariate_source` is not valid option")
      }
    }
  }
  if (arg_name == "covariate_date_made"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`covariate_date_made` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`covariate_date_made` can only be of length = 1")
      }
    }
  }
  if (arg_name == "cov_fcast"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`cov_fcast` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`cov_fcast` can only be of length = 1")
    }
  }
  if (arg_name == "cov_hist"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`cov_hist` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`cov_hist` can only be of length = 1")
    }
  }
  if (arg_name == "c_filename"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`c_filename` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`c_filename` can only be of length = 1")
    }
  }
  if (arg_name == "c_save"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`c_save` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`c_save` can only be of length = 1")
    }
  }
  if (arg_name == "data_name"){
    valid_names <- c("all", "controls", "covariates", "covariate_forecasts",
                     "moons", "metadata")
    if (!is.character(arg_value)){
      out <- c(out, "`data_name` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`data_name` can only be of length = 1")
    }
    if (!(all(arg_value %in% valid_names))){
      out <- c(out, "`data_name` is not valid option")
    }
  }
  if (arg_name == "dates"){
    if (!is.null(arg_value)){
      cast_dates2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (any(is.na(cast_dates2))){
        out <- c(out, "`dates` is not a Date")
      }
    }
  }
  if (arg_name == "df"){
    if (!("data.frame" %in% class(arg_value))){
      out <- c(out, "`df` not a data.frame")
    }
  }
  if (arg_name == "download"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`download` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`download` can only be of length = 1")
    }
  }
  if (arg_name == "download_existing_predictions"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`download_existing_predictions` is not logical")
    }
    if (length(arg_value) != 1){
      msg <- "`download_existing_predictions` can only be of length = 1"
      out <- c(out, msg)
    }
  }
  if (arg_name == "drop_spp"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`drop_spp` is not a character")
      }
    }
  }
  if (arg_name == "end"){
    if (!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        out <- c(out, "`end` is not numeric")
      } else if (any(arg_value < 1 )| any(arg_value %% 1 != 0)){
        out <- c(out, "`end` is not a positive integer")
      }
    }
  }
  if (arg_name == "ensemble"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`ensemble` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`ensemble` can only be of length = 1")
    }
  }
  if (arg_name == "extension"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`extension` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`extension` can only be of length = 1")
      }
      lext <- nchar(arg_value)
      spot <- rep(NA, lext)
      for(i in 1:lext){
        spot[i] <- substr(arg_value, i, i) == "."
      }
      if (sum(spot) != 1){
        out <- c(out, "`extension` is not an extension")
      }
    }
  }
  if (arg_name == "fcast_nms"){
    if (!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        out <- c(out, "`fcast_nms` is not numeric")
      } else if (any(arg_value < 0) | any(arg_value %% 1 != 0)){
        out <- c(out, "`fcast_nms` is not a non-negative integer")
      }
    }
  }
  if (arg_name == "filename"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`filename` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`filename` can only be of length = 1")
    }
  }
  if (arg_name == "from_date"){
    if (!is.null(arg_value)){
      if (length(arg_value) != 1){
        out <- c(out, "`from_date` can only be of length = 1")
      }
      from_date2 <- tryCatch(as.Date(arg_value), error = function(x){NA})
      if (any(is.na(from_date2))){
        out <- c(out, "`from_date` is not a Date")
      }
    }
  }
  if (arg_name == "from_zenodo"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`from_zenodo` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`from_zenodo` can only be of length = 1")
    }
  }
  if (arg_name == "future_moons"){
    if (!("moons" %in% class(arg_value))){
      out <- c(out, "`future_moons` is not an moons table")
    }
  }
  if (arg_name == "hind_step"){
    if (length(arg_value) != 1){
      out <- c(out, "`hind_step` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`hind_step` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`hind_step` is not a positive integer")
    }
  }
  if (arg_name == "hist_cov"){
    if (!("covariates" %in% class(arg_value))){
      out <- c(out, "`hist_cov` is not a covariates table")
    }
  }
  if (arg_name == "hist_fcast_file"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`hist_fcast_file` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`hist_fcast_file` can only be of length = 1")
    }
  }
  if (arg_name == "lag"){
    if(!is.null(arg_value)){
      if (length(arg_value) != 1){
        out <- c(out, "`lag` can only be of length = 1")
      }
      if (!is.numeric(arg_value)){
        out <- c(out, "`lag` is not numeric")
      } else if (any(arg_value < 0 | arg_value %% 1 != 0)){
        out <- c(out, "`lag` is not a non-negative integer")
      }
    }
  }
  if (arg_name == "lead"){
    if (length(arg_value) != 1){
      out <- c(out, "`lead` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`lead` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`lead` is not a positive integer")
    }
  }
  if (arg_name == "lead_time"){
    if (length(arg_value) != 1){
      out <- c(out, "`lead_time` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`lead_time` is not numeric")
    } else if (any(arg_value < 0 | arg_value %% 1 != 0)){
      out <- c(out, "`lead_time` is not a non-negative integer")
    }
  }
  if (arg_name == "level"){
    if (!is.null(arg_value)){    
      if (!is.character(arg_value)){
        out <- c(out, "`level` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`level` can only be of length = 1")
      }
      AC_funs <- c("plot_cov_RMSE_mod_spp", "plot_err_lead_spp_mods",
                   "plot_cast_point", "select_most_ab_spp", "select_casts",
                   "AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")
      ST_funs <- c("all_options")
      if (fun_name %in% AC_funs){
        if (!(all(arg_value %in% c("All", "Controls")))){
          out <- c(out, "`level` must be 'All' or 'Controls'")
        }
      }
      if (fun_name %in% ST_funs){
        if (!(arg_value %in% c("Site", "Treatment"))){
          out <- c(out, "`level` must be 'Site' or 'Treatment'")
        }
      }
    }
  }
  if (arg_name == "local_paths"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`local_path` is not a character")
    }
  }
  if (arg_name == "main"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`main` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`main` can only be of length = 1")
    }
  }
  if (arg_name == "metadata"){
    if (!("metadata" %in% class(arg_value))){
      out <- c(out, "`metadata` is not a metadata list")
    }
  }
  if (arg_name == "meta_filename"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`meta_filename` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`meta_filename` can only be of length = 1")
    }
  }
  if (arg_name == "meta_save"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`meta_save` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`meta_save` can only be of length = 1")
    }
  }
  if (arg_name == "min_lag"){
    if (length(arg_value) != 1){
      out <- c(out, "`min_lag` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`min_lag` is not numeric")
    } else if (any(arg_value < 0 | arg_value %% 1 != 0)){
      out <- c(out, "`min_lag` is not a non-negative integer")
    }
  }
  if (arg_name == "min_observed"){
    if (length(arg_value) != 1){
      out <- c(out, "`min_observed` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`min_observed` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`min_observed` is not a positive integer")
    }
  }
  if (arg_name == "min_plots"){
    if (length(arg_value) != 1){
      out <- c(out, "`min_plots` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`min_plots` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`min_plots` is not a positive integer")
    }
  }
  if (arg_name == "min_traps"){
    if (length(arg_value) != 1){
      out <- c(out, "`min_traps` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`min_traps` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`min_traps` is not a positive integer")
    }
  }
  if (arg_name == "model"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`model` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`model` can only be of length = 1")
      }
    }
  }
  if (arg_name == "models"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`models` is not a character")
      }
    }
  }
  if (arg_name == "model_set"){
    if (!is.null(arg_value)){
      if (!is.character(arg_value)){
        out <- c(out, "`model_set` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`model_set` can only be of length = 1")
      }
      if (!(all(arg_value %in% c("prefab", "wEnsemble")))){
        out <- c(out, "`model_set` must be 'prefab' or 'wEnsemble'")
      }
    }
  }
  if (arg_name == "mod_covariates"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`mod_covariates` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`mod_covariates` can only be of length = 1")
    }
  }
  if (arg_name == "mod_type"){
    if (arg_value != "pevGARCH"){
      out <- c(out, "only `pevGARCH` supported for `mod_type`")
    }
  }
  if (arg_name == "moons"){
    if (!("moons" %in% class(arg_value))){
      out <- c(out, "`moons` is not an moons table")
    }
  }
  if (arg_name == "msg"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`msg` is not a character")
      }
    }
  }
  if (arg_name == "m_filename"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`m_filename` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`m_filename` can only be of length = 1")
    }
  }
  if (arg_name == "m_save"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`m_save` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`m_save` can only be of length = 1")
    }
  }
  if (arg_name == "nadot"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`nadot` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`nadot` can only be of length = 1")
    }
  }
  if (arg_name == "ndates"){
    if (length(arg_value) != 1){
      out <- c(out, "`ndates` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`ndates` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`ndates` is not a positive integer")
    }
  }
  if (arg_name == "newmoonnumbers"){
    if (!is.null(arg_value)){
      if (!is.numeric(arg_value)){
        out <- c(out, "`newmoonnumbers` is not numeric")
      } else if (any(arg_value < 0) | any(arg_value %% 1 != 0)){
        out <- c(out, "`newmoonnumbers` is not a non-negative integer")
      }
    }
  }
  if (arg_name == "new_forecast_covariates"){
    if (!("covariates" %in% class(arg_value))){
      out <- c(out, "`new_forecast_covariates` is not a covariates table")
    }
  }
  if (arg_name == "nfcnm"){
    if (length(arg_value) != 1){
      out <- c(out, "`nfcnm` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`nfcnm` is not numeric")
    } else if (any(arg_value < 0 | arg_value %% 1 != 0)){
      out <- c(out, "`nfcnm` is not a non-negative integer")
    }
  }
  if (arg_name == "n_future_moons"){
    if (length(arg_value) != 1){
      out <- c(out, "`n_future_moons` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`n_future_moons` is not numeric")
    } else if (any(arg_value < 0 | arg_value %% 1 != 0)){
      out <- c(out, "`n_future_moons` is not a non-negative integer")
    }
  }
  if (arg_name == "options_all"){
    if (!("all_options" %in% class(arg_value))){
      out <- c(out, "`options_all` is not an all_options list")
    }
  }
  if (arg_name == "options_cast"){
    if (!("cast_options" %in% class(arg_value))){
      out <- c(out, "`options_cast` is not a cast_options list")
    }
  }
  if (arg_name == "options_covariates"){
    if (!("covariates_options" %in% class(arg_value))){
      out <- c(out, "`options_covariates` is not a covariates_options list")
    }
  }
  if (arg_name == "options_data"){
    if (!("data_options" %in% class(arg_value))){
      out <- c(out, "`options_data` is not a data_options list")
    }
  }
  if (arg_name == "options_dir"){
    if (!("dir_options" %in% class(arg_value))){
      out <- c(out, "`options_dir` is not a dir_options list")
    }
  }
  if (arg_name == "options_metadata"){
    if (!("metadata_options" %in% class(arg_value))){
      out <- c(out, "`options_metadata` is not a metadata_options list")
    }
  }
  if (arg_name == "options_model"){
    if (!("model_options" %in% class(arg_value))){
      out <- c(out, "`options_model` is not a model_options list")
    }
  }
  if (arg_name == "options_models"){
    if (!("models_options" %in% class(arg_value))){
      out <- c(out, "`options_models` is not a models_options list")
    }
  }
  if (arg_name == "options_moons"){
    if (!("moons_options" %in% class(arg_value))){
      out <- c(out, "`options_moons` is not a moons_options list")
    }
  }
  if (arg_name == "options_out"){
    if (!any(grepl("options", class(arg_value)))){
      out <- c(out, "`options_out` is not an options list")
    }
  }
  if (arg_name == "options_PortalData"){
    if (!("PortalData_options" %in% class(arg_value))){
      out <- c(out, "`options_PortalData` is not a PortalData_options list")
    }
  }
  if (arg_name == "options_predictions"){
    if (!("predictions_options" %in% class(arg_value))){
      out <- c(out, "`options_predictions` is not a predictions_options list")
    }
  }
  if (arg_name == "options_rodents"){
    if (!("rodents_options" %in% class(arg_value))){
      out <- c(out, "`options_rodents` is not a rodents_options list")
    }
  }
  if (arg_name == "output"){
    if (arg_value != "abundance"){
      out <- c(out, "only `abundance` supported for `output`")
    }
  }
  if (arg_name == "plots"){
    if (!is.character(arg_value)){
      out <- c(out, "`plots` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`plots` can only be of length = 1")
    }
    if (!(all(arg_value %in% c("all", "longterm")))){
      out <- c(out, "`plots` must be 'all' or 'longterm'")
    }
  }
  if (arg_name == "pred_dir"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`pred_dir` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`pred_dir` can only be of length = 1")
    }
  }
  if (arg_name == "quiet"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`quiet` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`quiet` can only be of length = 1")
    }
  }
  if (arg_name == "rangex"){
    if (length(arg_value) != 2){
      out <- c(out, "`rangex` can only be of length = 2")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`rangex` is not numeric")
    } else if (any(arg_value < 1) | any(arg_value %% 1 != 0)){
      out <- c(out, "`rangex` is not a positive integer")
    }
  }
  if (arg_name == "rodents"){
    if (!("rodents" %in% class(arg_value))){
      out <- c(out, "`rodents` is not of class rodents")
    }
  }
  if (arg_name == "rodents_list"){
    if (!("rodents_list" %in% class(arg_value))){
      out <- c(out, "`rodents_list` is not a rodents_list list")
    }
  }
  if (arg_name == "r_filename"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`r_filename` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`r_filename` can only be of length = 1")
    }
  }
  if (arg_name == "r_save"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`r_save` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`r_save` can only be of length = 1")
    }
  }
  if (arg_name == "save"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`save` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`save` can only be of length = 1")
    }
  }

  if (arg_name == "source_name"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`source_name` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`source_name` can only be of length = 1")
    }
  }
  if (arg_name == "species"){
    if(!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`species` is not a character")
      }
      if (!all(arg_value %in% rodent_spp("wtotal"))){
        out <- c(out, "invalid entry in `species`")
      } 
      sp_funs <- c("plot_cast_ts", "plot_cast_ts_ylab")
      if (fun_name %in% sp_funs){
        if (length(arg_value) != 1){
          out <- c(out, "`species` can only be of length = 1")
        }
      }
    }
  }
  if (arg_name == "species_set"){
    if (!is.null(arg_value)){
      if (!is.character(arg_value)){
        out <- c(out, "`species_set` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`species_set` can only be of length = 1")
      }
      if (!(all(arg_value %in% c("base", "wtotal", "evalplot")))){
        out <- c(out, "`species_set` must be 'base', 'wtotal', or 'evalplot'")
      }
    }
  }
  if (arg_name == "specific_subs"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`specific_subs` is not a character")
      }
    }
  }
  if (arg_name == "spp_names"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`spp_names` is not a character")
    }
  }
  if (arg_name == "start"){
    if (length(arg_value) != 1){
      out <- c(out, "`start` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`start` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`start` is not a positive integer")
    }
  }
  if (arg_name == "start_newmoon"){
    if (length(arg_value) != 1){
      out <- c(out, "`start_newmoon` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`start_newmoon` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`start_newmoon` is not a positive integer")
    }
  }
  if (arg_name == "subs"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`subs` is not a character vector")
    }
  }
  if (arg_name == "subs_names"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`subs_names` is not a character")
      }
    }
  }
  if (arg_name == "subs_type"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`subs_type` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`subs_type` can only be of length = 1")
      }
      if (!(all(arg_value %in% c("portalcasting")))){
        out <- c(out, "`subs_type` is not recognized ")
      }
    }
  }
  if (arg_name == "sub_path"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`sub_path` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`sub_path` can only be of length = 1")
      }
    }
  }
  if (arg_name == "tail"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`tail` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`tail` can only be of length = 1")
    }
  }
  if (arg_name == "temp_dir"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`temp_dir` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`temp_dir` can only be of length = 1")
    }
  }
  if (arg_name == "time"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`time` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`time` can only be of length = 1")
    }
  }
  if (arg_name == "tmnt_type"){
    if (!is.null(arg_value)){
      if (!is.character(arg_value)){
        out <- c(out, "`tmnt_type` is not a character")
      }
      if (length(arg_value) != 1){
        out <- c(out, "`tmnt_type` can only be of length = 1")
      }
      if (!(all(arg_value %in% c("all", "controls")))){
        out <- c(out, "`tmnt_type` must be 'all' or 'controls'")
      }
    }
  }
  if (arg_name == "to_cleanup"){
    if (!is.null(arg_value)){
      if (!("character" %in% class(arg_value))){
        out <- c(out, "`to_cleanup` is not a character")
      }
    }
  }
  if (arg_name == "topx"){
    if (length(arg_value) != 1){
      out <- c(out, "`topx` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`topx` is not numeric")
    } else if (any(arg_value < 1 | arg_value %% 1 != 0)){
      out <- c(out, "`topx` is not a positive integer")
    }
  }
  if (arg_name == "treatment"){
    if (!(is.null(arg_value)) && arg_value != "control"){
      out <- c(out, "`treatment` must be `NULL` or 'control'")
    }
  }
  if (arg_name == "tree"){
    if (!("dirtree" %in% class(arg_value))){
      out <- c(out, "`tree` is not a dirtree list")
    }
  }
  if (arg_name == "verbose"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`verbose` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`verbose` can only be of length = 1")
    }
  }
  if (arg_name == "version"){
    if (!("character" %in% class(arg_value))){
      out <- c(out, "`version` is not a character")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`version` can only be of length = 1")
    }
  }
  if (arg_name == "with_census"){
    if (!("logical" %in% class(arg_value))){
      out <- c(out, "`with_census` is not logical")
    }
    if (length(arg_value) != 1){
      out <- c(out, "`with_census` can only be of length = 1")
    }
  }
  if (arg_name == "x"){
    if (fun_name == "na_conformer"){
      if (!(is.data.frame(arg_value) | is.vector(arg_value))){
        out <- c(out, "`x` is not a data.frame or vector")
      }
    }
    if (fun_name == "classy"){
    }
  }
  if (arg_name == "yr"){
    if (length(arg_value) != 1){
      out <- c(out, "`yr` can only be of length = 1")
    }
    if (!is.numeric(arg_value)){
      out <- c(out, "`yr` is not numeric")
    } else if (any(arg_value < 1970 | arg_value %% 1 != 0)){
      out <- c(out, "`yr` is not an integer after 1970")
    }
  }
  out
}


