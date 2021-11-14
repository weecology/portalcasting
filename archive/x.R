  - title: "Date formatting"
    desc: "Functions to format dates and date components"
    contents:
      - foy
      - add_date_from_components
      - cast_window

      - clear_tmp

      - append_csv
      - combine_hist_and_cast
      - match.call.defaults
  - title: "Casting functions"
    desc: "Functions for fitting models and making predictions into the future"
    contents:
      - portalcast
      - cast
      - verify_models
      - models_to_cast
      - read_cast_tab
      - select_casts
      - update_models
  - title: "Cast processing functions"
    desc: "Functions for processing, evaluating, and combining casts"
    contents:
      - save_cast_output
      - ensemble_casts
      - measure_cast_level_error
      - add_lead_to_cast_tab
      - add_err_to_cast_tab
      - add_covered_to_cast_tab
      - add_obs_to_cast_tab


  - title: "Model helpers"
    desc: "Functions that help reduce code within model functions"
    contents:
      - covariate_models
      - lag_covariates
      - jags_ss 
      - runjags_control

  - title: "Prefabricated models"
    desc: "Functions of the pre-existing models"
    contents:
      - AutoArima
      - ESSS
      - jags_RW
      - nbGARCH
      - nbsGARCH
      - NaiveArima
      - pevGARCH



#' @rdname portalcast_messages
#'
#' @export
#' 
portalcast_welcome <- function (quiet = FALSE) {

  version_message(quiet = quiet)
  messageq("Preparing directory for casting", quiet = quiet)
  messageq(message_break(), quiet = quiet)

}

#' @rdname portalcast_messages
#'
#' @export
#'
portalcast_goodbye <- function (quiet = FALSE) {
  
  messageq(message_break(), quiet = quiet)
  messageq("Casting complete", quiet = quiet)
  messageq(message_break(), quiet = quiet)

}

#' @rdname portalcast_messages
#'
#' @export
#' 
data_readying_message <- function (end_moon = NULL,  
                                   quiet    = FALSE) {
  
  return_if_null(end_moon)
  messageq("Readying data for forecast origin newmoon ", end_moon, quiet = quiet)

}


#' @rdname portalcast_messages
#'
#' @export
#' 
data_resetting_message <- function (quiet = FALSE) {
  
  messageq(message_break(), quiet = quiet)
  messageq("Resetting data to the most up-to-date versions", quiet = quiet)

}




#' @rdname portalcast_messages
#'
#' @export
#' 
models_running_message <- function (end_moon = NULL,  
                                    quiet    = FALSE) {
  
  return_if_null(end_moon)
  messageq(message_break(), quiet = quiet)
  messageq("Running models for forecast origin newmoon ", end_moon, quiet = quiet)

}

#' @rdname portalcast_messages
#'
#' @export
#' 
model_running_message <- function (model = NULL,
                                   quiet = FALSE) {

  return_if_null(model)
  messageq(" - Running ", path_no_ext(basename(model)), quiet = quiet)

}

#' @rdname portalcast_messages
#'
#' @export
#' 
model_done_message <- function (model      = NULL,
                                run_status = NULL, 
                                quiet      = FALSE){

  return_if_null(model)
  return_if_null(run_status)

  modelname <- path_no_ext(basename(model))

  if (all(is.na(run_status))) {

    messageq("  |----| ", modelname, " failed |----|", quiet = quiet)

  } else {

    messageq("  |++++| ", modelname, " successful |++++|", quiet = quiet)

  }

}

  - title: "Path management"
    desc: "Utilities for quickly retrieving and generally formatting folder and file paths and saving conditions"
    contents:
      - main_path
      - sub_path
      - file_path
      - raw_path
      - data_path
      - casts_path
      - tmp_path
      - models_path

      - files_control

  - title: "Figures"
    desc: "Functions to make standard figures"
    contents:
      - plot_cast_point
      - plot_cast_ts
      - plot_casts_cov_RMSE
      - plot_casts_err_lead

#' @title Replace a value with an alternative if it is NULL or if it is NA
#'
#' @description 
#'  \code{ifnull} replaces the focal input with the alternative value if it
#'   is \code{NULL}. \cr \cr
#'  \code{ifna} replaces the focal input with the alternative value if it
#'   is \code{NA}.
#'
#' @param x Focal input.
#'
#' @param alt Alternative value.
#'
#' @return 
#'  \code{ifnull}: \code{x} if not \code{NULL}, \code{alt} otherwise. \cr \cr
#'  \code{ifna}:  \code{x} if not \code{NA}, \code{alt} otherwise. 
#' 
#' @examples
#'  ifnull(NULL, 123)
#'  ifnull(TRUE, 123)
#'  ifnull(FALSE, 123)
#'  ifna(NA, 123)
#'  ifna(FALSE, 123)
#'  ifna(NA, NA)
#'
#' @name alternative_values
#'
NULL

#' @rdname alternative_values
#'
#' @export 
#'
ifnull <- function (x = NULL, alt = NULL) {

  if(is.null(x)){
    x <- alt
  }
  x

}

#' @rdname alternative_values
#'
#' @export 
#'
ifna <- function (x = NULL, alt = NA) {

  ifelse(is.na(x), alt, x)

}


#' @title Save data out to a csv, appending the file if it already exists
#'
#' @description Appending a \code{.csv} without re-writing the header of the
#'  file. If the doesn't exist, it will be created.
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param filename \code{character} filename of existing \code{.csv} to be 
#'  appended.
#'
#' @return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   df <- data.frame(x = 1:10)
#'   fpath <- file_path(files = "xx.csv")
#'   append_csv(df, fpath)
#'  }
#'
#' @export
#'
append_csv <- function(df, filename){
  
  write.table(df, filename, sep = ",", row.names = FALSE, 
    col.names = !file.exists(filename), append = file.exists(filename))
  NULL
}

#' @title Calculate the fraction of the year from a date
#' 
#' @description Based on the year in which the date occurred, determine the
#'   fraction of the year (foy) for the date (in relation to New Year's Eve
#'   in that year). 
#'
#' @param dates \code{Date}(s) or \code{Date}-conformable value(s) to be 
#'   converted to the fraction of the year.
#'
#' @return \code{numeric} value(s) of the fraction of the year.
#'
#' @examples
#'  foy(Sys.Date())
#'
#' @export
#'
foy <- function(dates = NULL){
  return_if_null(dates)
  
  dates <- as.Date(dates)
  jday <- as.numeric(format(dates, "%j"))
  nye <- as.Date(paste0(format(dates, "%Y"), "-12-31"))
  nyejday <- as.numeric(format(nye, "%j"))
  round(jday / nyejday, 3)
}

#' @title Remove files from the tmp subdirectory
#'
#' @description Clear the files from the tmp subdirectory.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or just tidy messages. 
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  made or not. For toggling separately from the more general \code{quiet}
#'  argument. 
#'
#'
#' @return \code{NULL}, with the tmp subdirectory's files removed.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   clear_tmp
#'  }
#'
#' @export
#'
clear_tmp <- function(main = ".", bline = TRUE, quiet = FALSE, 
                      verbose = FALSE, cleanup = TRUE){
  
  tmp_path <- tmp_path(main = main)
  tmp_exist <- dir.exists(tmp_path)
  tmp_files <- list.files(tmp_path)
  ntmp_files <- length(tmp_files)
  if(!cleanup){
    return()
  }
  messageq(message_break(), quiet = quiet)
  messageq("Clearing tmp subdirectory", quiet = quiet)

  if(tmp_exist){
    if(ntmp_files > 0){
      tmp_files_full_paths <- file_path(main = main, sub = "tmp", 
                                        files = tmp_files)
      unlink(tmp_files_full_paths, force = TRUE, recursive = TRUE)
      msg <- "    *temporary files cleared from tmp subdirectory*"
    } else {
      msg <- "    *tmp subdirectory already clear*"
    }
  } else{
    msg <- "    *tmp subdirectory not present for clearing*"
  }
  messageq(msg, quiet = !verbose)
  NULL
}



#' @title Combine a historical table and a cast table
#'
#' @description A simple utility for combining a table of historical data
#'  and a table of cast data that might need to be assigned to either one
#'  or the other.
#'
#' @param hist_tab,cast_tab A pair of \code{data.frame}s with the same columns
#'  including a code{date} column of \code{Date}s, which is used to align 
#'  them.
#'
#' @param winner \code{character} value either {"hist"} or \code{"cast"} to
#'  decide who wins any ties. In the typical portalcasting space, this is 
#'  kept at its default value throughout. In the case of \code{NA} values,
#'  this will be overriden to use the entry that has no missing entries.
#'
#' @param column \code{character} indicating the column to use for identifying
#'  entries in combining.
#'
#'
#' @return \code{data.frame} combining \code{hist_tab} and \code{cast_tab}.
#' 
#' @examples
#'  hist_tab <- data.frame(date = seq(Sys.Date(), Sys.Date() + 5, 1), x = 1:6)
#'  cast_tab <- data.frame(date = seq(Sys.Date() + 5, Sys.Date() + 10, 1),
#'                         x = 101:106)
#'  combine_hist_and_cast(hist_tab, cast_tab, "hist") 
#'  combine_hist_and_cast(hist_tab, cast_tab, "cast")  
#'
#' @export
#'
combine_hist_and_cast <- function(hist_tab = NULL, cast_tab = NULL, 
                                  winner = "hist", column = "date"){
  
  return_if_null(hist_tab, cast_tab)
  return_if_null(cast_tab, hist_tab)

  hist_tab$x_source <- "hist"
  cast_tab$x_source <- "cast"
  out <- rbind(hist_tab, cast_tab)
  in_out <- rep(TRUE, NROW(out))
  dupes <- names(which(table(out[,column]) > 1))

  ndupes <- length(dupes) 
  if(ndupes > 0){
    for(i in 1:ndupes){
      which_duped <- which(out$moon == dupes[i])

      which_duped_hist <- which(as.character(out[,column]) == dupes[i] &
                                out$x_source == "hist")
      which_duped_cast <- which(as.character(out[,column]) == dupes[i] &
                                out$x_source == "cast") 

      hist_dupe_NA <- any(is.na(out[which_duped_hist, ]))
      cast_dupe_NA <- any(is.na(out[which_duped_cast, ]))

      if(winner == "hist"){
        if(!hist_dupe_NA){
          in_out[which_duped_cast] <- FALSE
        } else{
          in_out[which_duped_hist] <- FALSE   
        }
      } else if(winner == "cast"){
        if(!cast_dupe_NA){
          in_out[which_duped_hist] <- FALSE
        } else{
          in_out[which_duped_cast] <- FALSE   
        }
      }
    }
  }
  out <- out[ , -which(colnames(out) == "x_source")]
  out[in_out, ]
}

#' @title Add a date to a table that has the year month and day as components 
#' 
#' @description Add a date (as a \code{Date}) column to a table that has the 
#'  year month and day as components.
#' 
#' @param df \code{data.frame} with columns named \code{year}, \code{month},
#'  and \code{day}. 
#'
#'
#' @return \code{data.frame} \code{df} with column of \code{Date}s 
#'  named \code{date} added.
#'
#' @examples
#'  df <- data.frame(year = 2010, month = 2, day = 1:10)
#'  add_date_from_components(df)
#'
#' @export
#'
add_date_from_components <- function(df){
  
  yrs <- df$year
  mns <- df$month
  dys <- df$day
  df$date <- as.Date(paste(yrs, mns, dys, sep = "-"))
  df
}




#' @title Remove any specific incomplete entries as noted by an NA
#'
#' @description Remove any incomplete entries in a table, as determined by
#'  the presence of an \code{NA} entry in a specific column 
#'  (\code{colname}).
#'
#' @param df \code{data.frame} table to be written out.
#'
#' @param colname A single \code{character} value of the column to use
#'  to remove incomplete entries. 
#'
#' @return \code{df} without any incomplete entries. 
#'
#' @examples
#'  df <- data.frame(c1 = c(1:9, NA), c2 = 11:20)
#'  remove_incompletes(df, "c1")
#'
#' @export
#'
remove_incompletes <- function(df, colname){
  
  incompletes <- which(is.na(df[ , colname]))
  if (length(incompletes) > 0){
    df <- df[-incompletes, ]
  }
  df
}

#' @title Determine the depth of a list
#'
#' @description Evaluate an input for the depth of its nesting. 
#'
#' @details If \code{xlist = list()}, then technically the input value is a 
#'  list, but is empty (of length \code{0}), so depth is returned as \code{0}.
#'
#' @param xlist Focal input \code{list}.
#'
#' @return \code{integer} value of the depth of the list.
#' 
#' @examples
#'  list_depth("a")
#'  list_depth(list())
#'  list_depth(list("a"))
#'  list_depth(list(list("a")))
#'
#' @export 
#'
list_depth <- function(xlist){
  xx <- match.call()
  xxx <- deparse(xx[[2]])
  if(xxx == "list()"){
    0L
  } else if (is.list(xlist)){
    1L + max(sapply(xlist, list_depth))
  } else {
    0L
  }
}



#' @title Argument matching with defaults
#'
#' @description Expansion of \code{\link[base]{match.call}} to include
#'  default formal values.
#'
#' @param definition A \code{function}, by default the function from which 
#'  \code{match.call.defaults} is called. 
#'
#' @param call An unevaluated \code{call} to the function specified by 
#'  \code{definition}, as generated by \code{\link[base]{call}}.
#'
#' @param expand.dots \code{logical} defining if arguments matching 
#'  \code{...} in the call be included or left as a \code{...} argument
#'
#' @param envir An \code{environment}, from which the \code{...} in 
#'  \code{call} are retrieved, if any.
#'
#' @references 
#'  DesignLibrary's \code{match.call.defaults} function. \cr
#'  Stack overflow post reply by Roland. \href{https://bit.ly/2PtEgy1}{URL}
#'
#' @examples
#'  fun <- function(arg1 = "ok", ...) {
#'    match.call.defaults()
#'  }
#'  fun()
#'  fun(arg2 = "hi")
#'
#' @export
#'
match.call.defaults <- function(definition = sys.function(sys.parent()), 
                                call = sys.call(sys.parent()), 
                                expand.dots = TRUE, envir = parent.frame(2L)){
  call <- match.call(definition = definition, call = call, 
                     expand.dots = expand.dots, envir = envir)
  formals <- formals(fun = definition)
  if (expand.dots && "..." %in% names(formals)){
      formals[["..."]] <- NULL
  }
  diffs <- setdiff(names(formals), names(call))
  for (i in diffs){
    call[i] <- list(formals[[i]])
  }
  match.call(definition = definition, call = call, expand.dots = TRUE, 
             envir = envir)
}

#' @title Error if a function's request is deeper than can be handled
#'
#' @description Produces an informative error message when a function 
#'  that should only be called inside of other functions is called outside
#'  of a function (hence the request to the function is too deep for
#'  what it can handle).
#' 
#' @param lev The number of frames back in the stack where the request needs
#'  to be able to be evaluated.
#'
#' @return Throws an error if the function is called in a place where it 
#'  cannot operate and returns \code{NULL} otherwise.
#'
#' @examples
#'  \dontrun{
#'  # will error:
#'  # error_if_deep(-10)
#'  }
#'  error_if_deep(0)
#'
#' @export
#'
error_if_deep <- function(lev){
  lev2 <- lev - 1
  too_deep <- tryCatch(sys.call(lev2), error = function(x){NA})
  if(!is.null(too_deep) && !is.call(too_deep) && is.na(too_deep)){
    msg <- "too deep; function should only be called inside other functions"
    stop(msg, call. = FALSE)
  } 
}



#' @title Update models based on user input controls
#'
#' @description Update model scripts based on the user-defined model control
#'  inputs. This allows users to define their own models or re-define prefab
#'  models within the \code{\link{portalcast}} pipeline.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to update.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param update_prefab_models \code{logical} indicator if all of the models'
#'  scripts should be updated, even if they do not have an explicit change
#'  to their model options via \code{controls_model}. Default is
#'  \code{FALSE}, which leads to only the models in \code{controls_model}
#'  having their scripts re-written. Switching to \code{TRUE} results in the
#'  models listed in \code{models} having their scripts re-written. \cr \cr
#'  This is particularly helpful when one is changing the global (with respect
#'  to the models) options \code{main}, \code{quiet}, \code{verbose}, or
#'  \code{control_files}.
#'
#' @return \code{NULL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   cm <- model_control(name = "AutoArima", data_sets = "all")
#'   update_models(controls_model = cm)
#'  }
#'
#' @export
#'
update_models <- function(main = ".", models = NULL,
                          update_prefab_models = FALSE, 
                          control_files = files_control(),
                          quiet = FALSE, verbose = FALSE){

  if(update_prefab_models){
    models <- unique(c(models, prefab_models()))
  } 
  return_if_null(models)

  messageq("Updating model scripts", quiet = quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, 
                model = models[i])
  }
  messageq(message_break(), quiet = quiet) 
  invisible(NULL)
}



#' @title Verify that models requested to cast with exist
#'
#' @description Verify that models requested have scripts in the models 
#'   subdirectory. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param models \code{character} vector of the names of models to verify.
#'
#' @return \code{NULL} with messaging. 
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   verify_models()
#'  }
#'
#' @export
#'
verify_models <- function(main = ".", models = prefab_models(), 
                          quiet = FALSE){
  messageq("Checking model availability", quiet = quiet)
  model_dir <- sub_path(main = main, subs = "models")
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist", call. = FALSE)
  }
  available <- list.files(model_dir)
  if (models[1] != "all"){
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      msg <- paste0("Requested model(s) ", missmod, " not in directory \n")
      stop(msg, call. = FALSE)
    }
  }
  messageq(" *All requested models available*", quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)
}





#' @rdname read_data
#'
#' @export
#'
read_covariate_casts <- function(main = ".", settings = directory_settings(),
                           quiet = FALSE, verbose = FALSE){
  
  curr_path <- file.path(main, "data", control_files$filename_cov_casts)
  curr_path2 <- gsub("covariate_casts", "covariate_forecasts", curr_path)

  arch_path <- paste0(control_files$directory, "/data/", 
                      control_files$filename_cov_casts)
  arch_path <- file.path(main, "raw", arch_path)
  arch_path2 <- gsub("covariate_casts", "covariate_forecasts", arch_path)

  if(file.exists(curr_path)){
    cov_cast <- read.csv(curr_path, stringsAsFactors = FALSE)
  } else if (file.exists(curr_path2)){
    cov_cast <- read.csv(curr_path2, stringsAsFactors = FALSE)
  } else {
    if(file.exists(arch_path)){
      cov_cast <- read.csv(arch_path, stringsAsFactors = FALSE)
    } else if (file.exists(arch_path2)){
      cov_cast <- read.csv(arch_path2, stringsAsFactors = FALSE)
    } else {
      msg <- "current and archive versions missing, run `fill_raw`"
      stop(msg, call. = FALSE)
    }
  }
  if(any(grepl("forecast_newmoon", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("forecast_newmoon", "cast_moon", 
                                colnames(cov_cast))
  }
  if(any(grepl("newmoonnumber", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("newmoonnumber", "moon", colnames(cov_cast))
  } 
  cov_cast
}


\code{"covariate_casts"}, 

head(covariates)



  last_covar_moon <- max(c(covariates$moon, covariates$moon),
                         na.rm = TRUE)
  first_cast_covar_moon <- last_covar_moon + 1
  first_cast_rodent_moon <- last_rodent_moon + 1
  last_cast_moon <- end_moon + lead_time
  rodent_cast_moons <- first_cast_rodent_moon:last_cast_moon
  which_r_nms <- which(moons$moon %in% rodent_cast_moons)
  rodent_nm_dates <- as.Date(moons$moondate[which_r_nms])
  rodent_cast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_cast_years <- as.numeric(format(rodent_nm_dates, "%Y"))

  covar_cast_moons <- first_cast_covar_moon:last_cast_moon
  which_c_nms <- which(moons$moon %in% covar_cast_moons)
  covar_nm_dates <- as.Date(moons$moondate[which_c_nms])
  covar_cast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_cast_years <- as.numeric(format(covar_nm_dates, "%Y"))

  cast_type <- ifelse(end_moon == last_moon, "forecast", "hindcast")

  cast_meta <- read_casts_metadata(main = main, quiet = quiet)
  cast_group <- max(cast_meta$cast_group) + 1


  filename_config <- settings$files$directory_config
  filename_meta <- settings$files$metadata
           cast_group              = cast_group, 


           cast_type               = cast_type, 
           start_moon              = start_moon, 
           end_moon                = end_moon, 
           last_moon               = last_moon, 
           lead_time               = lead_time, 
           origin                  = as.character(origin), 
           covariate_cast_moons    = covar_cast_moons, 
           covariate_cast_months   = covar_cast_months, 
           covariate_cast_years    = covar_cast_years,
           rodent_cast_moons       = rodent_cast_moons, 
           rodent_cast_months      = rodent_cast_months, 
           rodent_cast_years       = rodent_cast_years,
           confidence_level        = settings$confidence_level

#' @title Determine specific newmoon numbers
#'
#' @description 
#'  \code{target_moons} determines which moons are in a window, based on a
#'  date and window width. \cr \cr
#'  \code{last_moon} determines the most recently passed newmoon, based on
#'  a date.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param date \code{Date} used for defining a window (starting with moons
#'  subsequent to the date) or a previous moon (before or on the date).
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return 
#'  \code{target_moons}: \code{numeric} vector of the moon numbers 
#'  targeted by the date window.
#'  \code{last_newmoon}:  \code{numeric} value of the last moon number.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   target_moons()
#'   last_moon()
#'  }
#'
#' @export
#'
target_moons <- function(moons = NULL, end_moon = NULL, 
                         lead_time = 12, date = Sys.Date()){

  return_if_null(moons)

  end_moon <- ifnull(end_moon, last_moon(moons = moons, date = date))

  (end_moon + 1):(end_moon + lead_time)

}
main = main, models = models,
                datasets = datasets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_model = controls_model,
                quiet = quiet, 
                control_files = control_files)

#' @rdname target_moons
#'
#' @export
#'
last_moon <- function(moons = NULL, date = Sys.Date()){

  return_if_null(moons)
  
  moons$newmoonnumber[max(which(moons$newmoondate < date))]

}
#' @title Trim a moons table based on target moons
#'
#' @description Some functions require that a data table of time (the moons)
#'  table only includes specific time stamps (newmoon numbers). This function
#'  is a simple utility to reduce the moons table to that as well as the 
#'  columns.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param target_moons \code{integer}-conformable newmoon numbers, usually
#'  made through \code{\link{target_moons}} of the moons to either 
#'  drop or retain based on \code{retain_target_moons}. 
#' 
#' @param retain_target_moons \code{logical} value that dictates if 
#'  \code{target_moons} are to be dropped (\code{retain_target_moons = FALSE}
#'  or retained (\code{retain_target_moons = TRUE})
#'
#' @param target_cols \code{character} vector of columns to retain.
#'
#' @return \code{moons} and a \code{data.frame} but trimmed.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   moons <- prep_moons()
#'   trim_moons(moons, 300:310)
#'  }
#'
#' @export
#'
trim_moons <- function(moons = NULL, target_moons = NULL, 
                       retain_target_moons = TRUE,
                       target_cols = c("moon", "moondate", 
                                       "period", "censusdate")){

  return_if_null(moons)
  moons <- moons[, target_cols]
  new_moons <- moons
  which_target_moons <- which(moons$moon %in% target_moons)
  ntarget_moons <- length(which_target_moons)
  if (ntarget_moons > 0){
    if(retain_target_moons){
      new_moons <- new_moons[which_target_moons, ]
    } else{
      new_moons <- new_moons[-which_target_moons, ]
    }
  }
  new_moons
}



#' @title Summarize a daily weather table by newmoons
#'
#' @description Summarizes a daily weather table by newmoons. Taking the 
#'  max date, min of the min temperatures, max of the max temperatures,
#'  mean of the mean temperatures, and sum of the precipitation.
#'
#' @param x \code{data.frame} of daily weather, with columns named
#'  \code{"date"}, \code{"mintemp"}, \code{"maxtemp"}, \code{"meantemp"}, 
#'  \code{"precipitation"}, and \code{"moon"}.
#'
#' @return \code{data.frame} of \code{x} summarized and arranged by
#'  \code{x$moon}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   raw_path <- raw_path()
#'   moons <- prep_moons()
#'   weather <- portalr::weather("daily", TRUE, raw_path)
#'   weather <- add_date_from_components(weather)
#'   weather <- add_moons_from_date(weather, moons)
#'   summarize_daily_weather_by_moon(weather)
#'  }
#'
#' @export
#'
summarize_daily_weather_by_moon <- function(x){

  x <- x[!(is.na(x$moon)), ] 
  umoons <- unique(x$moon)
  numoons <- length(umoons)
  date <- rep(NA, numoons)
  mintemp <- rep(NA, numoons)
  maxtemp <- rep(NA, numoons)
  meantemp <- rep(NA, numoons)
  precipitation <- rep(NA, numoons)
  ndvi <- rep(NA, numoons)
  for(i in 1:numoons){
    moons_in <- x$moon == umoons[i]
    date[i] <- max(as.Date(x$date[moons_in], na.rm = TRUE))
    mm <- na.omit(x$mintemp[moons_in])
    if(length(mm) > 0){
      mintemp[i] <- min(mm, na.rm = TRUE)
    } else{
      mintemp[i] <- NA
    }

    mm <- na.omit(x$maxtemp[moons_in])
    if(length(mm) > 0){
      maxtemp[i] <- max(mm, na.rm = TRUE)
    } else{
      maxtemp[i] <- NA
    }

    mm <- na.omit(x$meantemp[moons_in])
    if(length(mm) > 0){
      meantemp[i] <- mean(mm, na.rm = TRUE)
    } else{
      meantemp[i] <- NA
    }

    pp <- na.omit(x$precipitation[moons_in])
    if(length(mm) > 0){
      precipitation[i] <- sum(pp, na.rm = TRUE)
    } else{
      precipitation[i] <- NA
    }

    nn <- na.omit(x$ndvi[moons_in])
    if(length(nn) > 0){
      ndvi[i] <- sum(nn, na.rm = TRUE)
    } else{
      ndvi[i] <- NA
    }

  }
  out <- data.frame(moon = umoons, mintemp, maxtemp, meantemp, precipitation,
                    ndvi)
  moon_order <- order(out$moon)
  out[moon_order, ]
}



#' @title Lag covariate data
#'
#' @description Lag the covariate data together based on the new moons
#'
#' @param covariates \code{data.frame} of covariate data to be lagged. 
#'  
#' @param lag \code{integer} lag between rodent census and covariate data, in
#'  new moons.
#'  
#' @param tail \code{logical} indicator if the data lagged to the tail end 
#'  should be retained.
#'
#' @return \code{data.frame} with a \code{newmoonnumber} column reflecting
#'  the lag.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   covariate_casts <- read_covariate_casts()
#'   covar_casts_lag <- lag_covariates(covariate_casts, lag = 2, tail = TRUE)
#'  }
#'
#' @export
#'
lag_covariates <- function(covariates, lag, tail = FALSE){

  covariates$moon_lag <- covariates$moon + lag
  
  if(tail == FALSE){
    oldest_included_moon <- covariates$moon[1]
    most_recent_moon <- covariates$moon[nrow(covariates)]
    hist_moons <- oldest_included_moon:most_recent_moon
    moon_match <- match(covariates$moon_lag, hist_moons)
    covariates$moon <- hist_moons[moon_match]
    if (lag > 0){
      covariates <- covariates[-(1:lag), ]
    }
  }
  covariates$moon <- covariates$moon_lag
  covariates$moon_lag <- NULL
  covariates
}

#' @title Cast the covariates
#'
#' @description Cast the covariates for a model run. \cr \cr
#'  \code{prep_cast_covariates} is the primary function, which produces the 
#'  covariates (min, mean, and max temperature; precipitation; and NDVI) 
#'  required for a model to be cast. Saves the cast data out via
#'  \code{save_cast_cov_csv} and tidying the data for use within the model 
#'  (by removing the vestigial columns that are only needed for saving). 
#'
#' @param main \code{character} name of the main directory tree component.
#'
#' @param moons Lunar data \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param hist_cov \code{data.frame} of historic covariates. See 
#'  \code{\link{prep_hist_covariates}}.
#'
#' @param cast_cov \code{data.frame} of cast covariates. See
#'  \code{\link{cast_covariates}}.
#'
#' @param lead_time \code{integer}-conformable number of timesteps forward 
#'  a cast will proceed.
#'
#' @param min_lag \code{integer}-conformable minimum non-0 covariate lag time 
#'  used in any model.
#'
#' @param cast_date \code{Date} on which the cast happens. Generally should
#'  be \code{\link{Sys.Date}}.
#'
#' @param end_moon Forecast origin. \code{integer}-conformable newmoon number 
#'  or \code{NULL}, which equates to the most recently included census'
#'  newmoon number. 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @details If forecasting from the current newmoon, casts the covariates 
#'  If casting from a previous newmoon, the
#'  historical forecasted weather and NDVI data are retrieved from the file 
#'  pointed to by \code{filename_cov_casts}.
#'
#' @return 
#'  \code{prep_cast_covariates}: \code{data.frame} of the cast covariates
#'   after saving, with excess columns removed. \cr \cr
#'  \code{save_cast_cov_csv}: \code{cast_cov} \code{data.frame} exactly as 
#'   input. 
#'  
#' @examples
#'  \donttest{
#'   setup_dir()
#'   hist_cov <- prep_hist_covariates()
#'   cast_cov <- prep_cast_covariates(hist_cov = hist_cov)
#'   save_cast_cov_csv(cast_cov = cast_cov)
#'  }
#'
#' @name cast_covariates
#'




#' @rdname cast_covariates
#'
#' @export
#'
save_cast_cov_csv <- function(main = ".", moons = NULL,
                              end_moon = NULL, cast_date = Sys.Date(),
                              cast_cov = NULL, 
                              control_files = files_control(),
                              quiet = TRUE, verbose = FALSE){

  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files))
  return_if_null(cast_cov)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date)
  end_moon <- ifnull(end_moon, last_moon)
  if(!control_files$save | !control_files$append_cast_csv | 
     !control_files$overwrite | end_moon != last_moon){
    return(cast_cov)
  }
  new_cast <- cast_cov
  new_cast$source <- control_files$source_name
  date_made <- Sys.time()
  tz <- format(date_made, "%Z")
  new_cast$date_made <- paste0(date_made, " ", tz)

  arch_path <- paste0(control_files$directory, "/data/", 
                      control_files$filename_cov_casts)
  arch_path <- file_path(main = main, sub = "raw", files = arch_path)
  arch_path2 <- gsub("covariate_casts", "covariate_forecasts", arch_path)

  if (file.exists(arch_path) | file.exists(arch_path2)) {
    hist_cast <- read_covariate_casts(main = main, 
                                      control_files = control_files,
                                      quiet = quiet, verbose = verbose)
    if(!("date" %in% colnames(hist_cast))){
      hist_cast$date <- NA
    }
  
    out <- rbind(hist_cast, new_cast)

  } else {

   out <- new_cast

  }

  out$date <- as.character(out$date)
  out_path <- file_path(main = main, sub = "data", 
                        files = control_files$filename_cov_casts)
  msg <- "    **covariates_casts.csv saved**"
  messageq(msg, quiet = !verbose)
  write.csv(out, out_path, row.names = FALSE)
  cast_cov
}




                                   min_lag = min_lag, 


#
# patching NDVI 
#

  na_ndvi <- is.na(out$ndvi)

  if (na_ndvi[nrow(out)]) {

    last_good <- max(which(!na_ndvi))
    full_length <- nrow(out)
    window_length <- full_length - last_good

    ndvi_fit <- auto.arima(out$ndvi)
    ndvi_cast <- forecast(ndvi_fit, h = window_length)
    out$ndvi[(last_good + 1):full_length] <- as.numeric(ndvi_cast$mean)

  }

  data.frame(out)


 moons = data_m, end_moon = end_moon, 
                            start_moon = start_moon, lead_time = lead_time, 
                            min_lag = min_lag, cast_date = cast_date, 
                            quiet = quiet, control_files = control_files)

#' @title Prepare historical covariates data
#'
#' @description 

#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param moons Lunar data \code{data.frame}. See \code{\link{prep_moons}}.
#'
#'
#' @return 
#'  \code{prep_hist_covariates}: daily historical covariate data table as a 
#'   \code{data.frame}. 
#'
#' @examples
#'  \donttest{   
#'   create_dir()
#'   fill_raw()
#'   prep_hist_covariates()
#'  }
#'
#' @name prepare_historical_covariates
#'
NULL


#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.


\code{list} of datasets to be created using \code{\link{do.call}} on the defined functions. 

#'              If the requested data do not exist, an effort is made to prepare them using the associated \code{prep_<data_name>} functions (like \code{prep_moons}).\cr \cr

names(prefab_rodent_datasets())
names(prefab_rodent_datasets())

 main = ".", models = prefab_models(),
                          datasets = NULL, moons = NULL, rodents = NULL,
                          covariates = NULL, end_moon = NULL, 
                          start_moon = 217, lead_time = 12, min_lag = 6, 
                          cast_date = Sys.Date(), confidence_level = 0.95, 
                          controls_model = NULL, 
                          control_files = files_control(),
                          quiet = TRUE, verbose = FALSE
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param rodents Rodents \code{list}. See \code{\link{prep_rodents}},
#'  
#' @param datasets \code{character} vector of the rodent data set names
#'  that the model is applied to. 
#'
#' @param covariates Covariates \code{data.frame}. See 
#'  \code{\link{prep_covariates}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param controls_model Additional controls for models not in the prefab
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.




lead_time = 12, cast_date = Sys.Date(), 
                       control_files = files_control()


            end_moon = end_moon,
            start_moon = start_moon,
            lead_time = lead_time, min_lag = min_lag,
            confidence_level = confidence_level,
            cast_date = cast_date,
                      controls_model = controls_model,
                      control_files = control_files
)

#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
#'
#' @param PortalData_source,PortalData_version \code{character} values for the
#'        source and version of the Portal Data to download. 
#'        \cr \cr 
#'        Default values retrieve the latest data from github.
#'        \cr \cr 
#'        See \code{\link[portalr]{download_observations}}.
#'
#' @param portalPredictions_source,portalPredictions_version \code{character} 
#'        values for the source and version of the archive to download.
#'        \cr \cr 
#'        Default values point to github, but \code{version = NULL} indicates
#'        no download.
#'        \cr \cr 
#'        See \code{\link{download_archive}}.

#'
#'
#' @param climate_forecast_source,climate_forecast_version  \code{character} 
#'        values for the source and version of the climate forecasts to
#'        download.
#'        \cr \cr 
#'        Default values retrieve the current day's forecast from the
#'        Northwest Knowledge Network's North American Multi-Model Ensemble 
#'        (NMME) climate forecasts.
#'        \cr \cr 
#'        See \code{\link{download_climate_forecasts}}.
#'
#' @param models \code{vector} of models to be written via 
#'        \code{\link{write_model}}.
#'
#' @param datasets \code{list} of datasets to be created using 
#'        \code{\link{do.call}} on the defined functions. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{datasets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{datasets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.


(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_model = NULL,

                          control_files = files_control(),
                      datasets = prefab_rodent_datasets(),
                          quiet = FALSE, verbose = TRUE){






#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(main = ".", models = prefab_models(), 
                             end_moon = NULL, start_moon = 217, 
                             lead_time = 12, confidence_level = 0.95, 
                             cast_date = Sys.Date(), controls_model = NULL,

                             control_files = files_control(), 
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = "latest",
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                             quiet = FALSE, verbose = TRUE, 
                      datasets = prefab_rodent_datasets()){
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
            quiet = quiet, verbose = verbose, 
                      datasets = datasets,
                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
            control_files = control_files)
}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_model = NULL,

                          control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      datasets = prefab_rodent_datasets(),
                          quiet = FALSE, verbose = TRUE){
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
                             PortalData_version = PortalData_version,
                      datasets = datasets,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
quiet = quiet, verbose = verbose, 
            control_files = control_files)
  sandbox_welcome(main = main, quiet = quiet)
}


                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
               models = models, 
                      datasets = datasets,
           controls_model = controls_model, 
           control_files = control_files, 

           end_moon = end_moon, lead_time = lead_time, 
           cast_date = cast_date, start_moon = start_moon, 
           confidence_level = confidence_level, 
           quiet = quiet, verbose = verbose)



end_moon = NULL, 
                      start_moon = 217, lead_time = 12, 
                      confidence_level = 0.95, cast_date = Sys.Date(),
                      controls_model = NULL, 
                      control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),

#'
_dir <- function (main  = ".",
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      models = prefab_models(),
                      datasets = prefab_rodent_datasets(),
                      quiet = FALSE,
                      verbose = TRUE,
                        controls_model = NULL, 
                        control_files = files_control(), 
                        end_moon = NULL, start_moon = 217, lead_time = 12,
 min_lag = 6, 
                        confidence_level = 0.95, cast_date = Sys.Date())
                        
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_model Additional controls for models not in the prefab
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 


#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param filename \code{character} value of the path to the directory config YAML.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param downloads_versions \code{character} vector returned from \code{\link{fill_raw}} of the successfully downloaded versions of downloaded data.


files_control <- function(directory = "portalPredictions",
                          raw_data = "PortalData",
                          filename_moons = ,
                          filename_config = "dir_config.yaml", 
                          filename_cov = "covariates.csv", 
                          filename_cov_casts = "covariate_casts.csv",
                          filename_meta = "metadata.yaml", 
                          save = TRUE, overwrite = TRUE, cleanup = TRUE,
                          source_name = "current_archive",
                          append_cast_csv = TRUE){
  
  list(directory = directory, raw_data = raw_data, 
       filename_moons = filename_moons, filename_cov = filename_cov,
       filename_cov_casts = filename_cov_casts,
       filename_config = filename_config, filename_meta = filename_meta,
       save = save, overwrite = overwrite, cleanup = cleanup,
       source_name = source_name, append_cast_csv = append_cast_csv)

}


#'
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'                  data directory. A standard portalcasting directory
#'                  downloads the raw data files into from the PortalData 
#'                  repository, so \code{raw_data = "PortalData"}.

#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'

#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts later use.
#'
#' @param source \code{character} value for the name to give the covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
