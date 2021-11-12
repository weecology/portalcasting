#' @title Prepare lunar data for the portalcasting repository
#'
#' @description A set of functions that get time information (calendar dates, 
#'  census periods, and newmoon numbers) associated with trapping events 
#'  (achieved and missed) based on a lunar survey schedule. If needed, 
#'  additional moons will be added to both the in-use and raw versions of 
#'  the data table. \cr \cr
#'  \code{add_future_moons} adds future moon dates to the moon table, counting 
#'  forward from \code{cast_date}. Because the \code{moons} table might not 
#'  have the most recent moons, more rows than \code{lead_time} may need to 
#'  be added to the table. \cr \cr. \cr \cr
#'  \code{add_extra_future_moons} adds more more moons to accomplish required
#'  data for \code{lead_time}. Because the moon table might not have the most 
#'  recent moons, more rows than initially requested may need to be added to 
#'  the table for it to cover \code{lead_time} .\cr \cr
#'  \code{format_moons} formats the final output table with \code{year} and
#'  \code{month} columns formatted accordingly, and the \code{moondate}
#'  column formatted as a \code{\link{Date}}. 
#'
#' @details Sometimes the raw moon data table is not fully up-to-date. Because
#'  the \code{portalr} functions \code{\link[portalr]{weather}} and 
#'  \code{\link[portalr]{fcast_ndvi}} point to the raw moons data, that table
#'  needs to be updated to produce the correct current data table for 
#'  casting. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param cast_date \code{Date} of the cast, typically today's date (set 
#'  using \code{\link{Sys.Date}}).
#'
#' @param moons Moons \code{data.frame}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#' 
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return All functions here return some version of a moons \code{data.frame}
#'  \cr \cr. 
#'  \code{prep_moons}, \code{format_moons}: fully appended and formatted 
#'  \code{data.frame} (also saved out if \code{save = TRUE}). \cr \cr
#'  \code{add_future_moons} and \code{add_extra_future_moons}: appropriately 
#'  appended moons
#'  \code{data.frame}.
#'   
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   prep_moons()
#'   pth <- file_path(files = "raw/PortalData/Rodents/moon_dates.csv")
#'   moons <- read.csv(pth, stringsAsFactors = FALSE)
#'   moons <- add_future_moons(moons)
#'   format_moons(moons)
#'  }
#'
#' @name prepare_moons
#'
NULL

#' @rdname prepare_moons
#'
#' @export
#' 
prep_moons <- function(main = ".", lead_time = 12, cast_date = Sys.Date(), 
                       control_files = files_control(), 
                       quiet = TRUE, verbose = FALSE){

  raw_data_present <- verify_raw_data(main = main, 
                                      raw_data = control_files$raw_data)
  if(!raw_data_present){
    fill_raw(main = main, quiet = quiet)
  }
  messageq("  -lunar data file", quiet)
  raw_path <- raw_path(main = main)
  traps_in <- load_trapping_data(path = raw_path, download_if_missing = FALSE,
                                 clean = FALSE, quiet = !verbose)
  moons_in <- traps_in[["newmoons_table"]]
  moons <- add_future_moons(main = main, moons = moons_in, 
                            lead_time = lead_time, cast_date = cast_date)
  moons_out <- format_moons(moons)
  write_data(dfl = moons_out, main = main, save = control_files$save, 
             filename = control_files$filename_moons, 
             overwrite = control_files$overwrite, 
             quiet = !verbose)
} 

#' @rdname prepare_moons
#'
#' @export
#'
format_moons <- function(moons, arg_checks = TRUE){
  check_args(arg_checks)
  moons$year <- as.numeric(format(as.Date(moons$newmoondate), "%Y"))
  moons$month <- as.numeric(format(as.Date(moons$newmoondate), "%m"))
  moons$newmoondate <- as.Date(moons$newmoondate)
  colnames(moons)[which(colnames(moons) == "newmoonnumber")] <- "moon"
  colnames(moons)[which(colnames(moons) == "newmoondate")] <- "moondate"

  moons
}

#' @rdname prepare_moons
#'
#' @export
#'
add_future_moons <- function(main = ".", moons = NULL, lead_time = 12, 
                             cast_date = Sys.Date(), 
                             control_files = files_control(),
                             arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  check_args(arg_checks)
  if(lead_time == 0){
    return(moons)
  }
  future_moons <- get_future_moons(moons, lead_time)
  future_moons <- add_extra_future_moons(future_moons, cast_date)
  future_moons$newmoondate <- as.character(future_moons$newmoondate)
  rbind(moons, future_moons)
}

#' @rdname prepare_moons
#'
#' @export
#'
add_extra_future_moons <- function(moons, cast_date = Sys.Date(), 
                                   arg_checks = TRUE){
  check_args(arg_checks)
  n_extra_future_moons <- length(which(moons$newmoondate < cast_date))
  if (n_extra_future_moons > 0){
    extra_moons <- get_future_moons(moons, n_extra_future_moons)
    moons <- rbind(moons, extra_moons)
  } 
  moons
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
                                       "period", "censusdate"), 
                       arg_checks = TRUE){
  check_args(arg_checks)
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

#' @title Add a newmoon number to a table that has the date
#' 
#' @description Add a \code{moon} (newmoon number) column to a table that has 
#'  a \code{date} (as a \code{Date}) column.
#' 
#' @param df \code{data.frame} with column of newmoon \code{Date}s 
#'  named \code{date}.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#' 
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{data.frame} \code{x} with column of \code{newmoonnumber}s 
#'  added.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
#'   moons <- prep_moons()
#'   raw_path <- raw_path()
#'   weather <- portalr::weather("daily", fill = TRUE, path = raw_path)
#'   add_moons_from_date(weather, moons)
#'  }
#'
#' @export
#'
add_moons_from_date <- function(df, moons = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(moons, df)
  moon_number <- moons$moon[-1]
  moon_start <- as.Date(moons$moondate[-nrow(moons)])
  moon_end <- as.Date(moons$moondate[-1])
  moon_match_number <- NULL
  moon_match_date <- NULL

  for (i in seq(moon_number)) {
    temp_dates <- seq.Date(moon_start[i] + 1, moon_end[i], 1)
    temp_dates <- as.character(temp_dates)
    temp_numbers <- rep(moon_number[i], length(temp_dates))
    moon_match_date <- c(moon_match_date, temp_dates)
    moon_match_number <- c(moon_match_number, temp_numbers)
  }
  moon_match_date <- as.Date(moon_match_date)
  if (is.null(df$date)){
    df <- add_date_from_components(df)
  }
  matches <- match(df$date, moon_match_date)
  df$moon <- moon_match_number[matches]
  df
}

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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
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
target_moons <- function(main = ".", moons = NULL, end_moon = NULL, 
                         lead_time = 12, date = Sys.Date(), 
                         control_files = files_control(),
                         arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  check_args(arg_checks)
  last_moon <- last_moon(main = main, moons = moons, date = date, 
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  (end_moon + 1):(end_moon + lead_time)
}

#' @rdname target_moons
#'
#' @export
#'
last_moon <- function(main = ".", moons = NULL, date = Sys.Date(), 
                      control_files = files_control(), arg_checks = TRUE){
  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  which_last_moon <- max(which(moons$moondate < date))
  moons$moon[which_last_moon]
}

