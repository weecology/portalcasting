#' @title Prepare lunar data for the portalcasting repository
#'
#' @description A set of functions that get time information (calendar dates, 
#'  census periods, and newmoon numbers) associated with trapping events 
#'  (achieved and missed) based on a lunar survey schedule. If needed, 
#'  additional moons will be added to both the in-use and raw versions of 
#'  the data table. \cr \cr
#'  \code{add_past_moons_to_raw} appends missing past moon dates to the
#'  raw data file. (See \code{Details}.) \cr \cr
#'  \code{add_future_moons} adds future moon dates to the moon table, counting 
#'  forward from \code{cast_date}. Because the \code{moons} table might not 
#'  have the most recent moons, more rows than \code{lead_time} may need to 
#'  be added to the table. \cr \cr. \cr \cr
#'  \code{add_extra_future_moons} adds more more moons to accomplish required
#'  data for \code{lead_time}. Because the moon table might not have the most 
#'  recent moons, more rows than initially requested may need to be added to 
#'  the table for it to cover \code{lead_time} .\cr \cr
#'  \code{format_moons} formats the final output table with \code{year} and
#'  \code{month} columns formatted accordingly, and the \code{newmoondate}
#'  column formatted as a \code{\link{Date}}. 
#'
#' @details Sometimes the raw moon data table is not fully up-to-date. Because
#'  the \code{portalr} functions \code{\link[portalr]{weather}} and 
#'  \code{\link[portalr]{fcast_ndvi}} point to the raw moons data, that table
#'  needs to be updated to produce the correct current data table for 
#'  casting. \code{add_past_moons_to_raw} updates the raw file accordingly. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param cast_date \code{Date} from which future is defined, typically 
#'   today's date (set using \code{\link{Sys.Date}}).
#'
#' @param moons Moons \code{data.frame}.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#' 
#' @param raw_path_data \code{character} value indicating the folder path
#'  to the data within the \code{raw} subdirectory but above the files. A 
#'  standard portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so \code{raw_path_data = "PortalData"} (as
#'  \code{"raw/"} is implied). 
#'
#' @param raw_moons_file \code{character} value indicating the path
#'  to the moons data file within \code{raw_path_data}. A standard 
#'  portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so 
#'  \code{raw_moons_file = "Rodents/moon_dates.csv"}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param filename_moons \code{character} name of the file for saving the 
#'  data output.
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
#' @return All functions here return some version of a moons \code{data.frame}
#'  \cr \cr. 
#'  \code{prep_moons}, \code{format_moons}: fully appended and formatted 
#'  \code{data.frame} (also saved out if \code{save = TRUE}). \cr \cr
#'  \code{add_past_moons_to_raw}, \code{add_future_moons}, 
#'  \code{add_extra_future_moons}: appropriately appended moons
#'  \code{data.frame}.
#'   
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   prep_moons()
#'   fpath <- file_paths("raw/PortalData/Rodents/moon_dates.csv")
#'   moons <- read.csv(fpath, stringsAsFactors = FALSE)
#'   moons <- add_future_moons(moons)
#'   moons <- add_past_moons_to_raw(moons)
#'   format_moons(moons)
#'  }
#'
#' @export
#' 
prep_moons <- function(main = ".", lead_time = 12, cast_date = Sys.Date(), 
                       raw_path_data = "PortalData",
                       raw_moons_file = "Rodents/moon_dates.csv",
                       quiet = FALSE, save = TRUE, 
                       overwrite = TRUE, filename_moons = "moon_dates.csv",
                       arg_checks = TRUE){
  check_args(arg_checks)
  lpath <- paste0("raw/", raw_path_data, "/", raw_moons_file)
  moon_path <- file_paths(main, lpath)
  moons_in <- read.csv(moon_path, stringsAsFactors = FALSE)
  moons <- add_future_moons(moons = moons_in, lead_time = lead_time, 
                            cast_date = cast_date, arg_checks = arg_checks)
  add_past_moons_to_raw(main = main, moons = moons_in,
                        cast_date = cast_date, 
                        raw_path_data = raw_path_data,
                        raw_moons_file = raw_moons_file, 
                        arg_checks = arg_checks)

  moons_out <- format_moons(moons)
  data_out(main = main, dfl = moons_out, save = save, 
           filename = filename_moons, overwrite = overwrite, quiet = quiet,
           arg_checks = arg_checks)
}

#' @rdname prep_moons
#'
#' @export
#'
format_moons <- function(moons, arg_checks = TRUE){
  check_args(arg_checks)
  moons$year <- year(moons$newmoondate)
  moons$month <- month(moons$newmoondate)
  moons$newmoondate <- as.Date(moons$newmoondate)
  moons
}

#' @rdname prep_moons
#'
#' @export
#'
add_future_moons <- function(moons = NULL, lead_time = 12, 
                             cast_date = Sys.Date(), arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  if(lead_time == 0){
    return(moons)
  }
  get_future_moons(moons, lead_time) %>%
  add_extra_future_moons(cast_date) %>%
  mutate(newmoondate = as.character(newmoondate)) %>%
  bind_rows(moons, .)
}

#' @rdname prep_moons
#'
#' @export
#'
add_extra_future_moons <- function(moons, cast_date = Sys.Date(), 
                                   arg_checks = TRUE){
  check_args(arg_checks)
  n_extra_future_moons <- length(which(moons$newmoondate < cast_date))
  if (n_extra_future_moons > 0){
    extra_moons <- get_future_moons(moons, n_extra_future_moons)
    moons <- bind_rows(moons, extra_moons)
  } 
  moons
}

#' @rdname prep_moons
#'
#' @export
#'
add_past_moons_to_raw <- function(main = ".", moons, cast_date = Sys.Date(),
                                  raw_path_data = "PortalData",
                                  raw_moons_file = "Rodents/moon_dates.csv",
                                  overwrite = TRUE, arg_checks = TRUE){
  check_args(arg_checks)
  if(overwrite){
    lpath <- paste0("raw/", raw_path_data, "/", raw_moons_file)
    fpath <- file_paths(main, lpath) 
    included_moons <- moons$newmoondate < cast_date
    new_raw <- moons[included_moons, ]
    write.csv(new_raw, fpath, row.names = FALSE)
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
#'  made through \code{\link{target_newmoons}} of the moons to either 
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
                       target_cols = c("newmoonnumber", "newmoondate", 
                                       "period", "censusdate"), 
                       arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(moons)
  moons <- moons[, target_cols]
  new_moons <- moons
  which_target_moons <- which(moons$newmoonnumber %in% target_moons)
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
#' @description Add a newmoon number column to a table that has a 
#'  \code{date} (as a \code{Date}) column.
#' 
#' @param df \code{data.frame} with column of newmoon \code{Date}s 
#'  named \code{date}.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
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
#' @return \code{data.frame} \code{x} with column of \code{newmoonnumber}s 
#'  added.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
#'   moons <- prep_moons()
#'   raw_path <- sub_paths(specific_subs = "raw")
#'   weather <- portalr::weather("daily", TRUE, raw_path)
#'   add_newmoons_from_date(weather, moons)
#'  }
#'
#' @export
#'
add_newmoons_from_date <- function(df, moons = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(moons, df)
  newmoon_number <- moons$newmoonnumber[-1]
  newmoon_start <- as.Date(moons$newmoondate[-nrow(moons)])
  newmoon_end <- as.Date(moons$newmoondate[-1])
  newmoon_match_number <- NULL
  newmoon_match_date <- NULL

  for (i in seq(newmoon_number)) {
    temp_dates <- seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1)
    temp_dates <- as.character(temp_dates)
    temp_numbers <- rep(newmoon_number[i], length(temp_dates))
    newmoon_match_date <- c(newmoon_match_date, temp_dates)
    newmoon_match_number <- c(newmoon_match_number, temp_numbers)
  }
  newmoon_match_date <- as.Date(newmoon_match_date)
  if (is.null(df$date)){
    df <- add_date_from_components(df)
  }
  matches <- match(df$date, newmoon_match_date)
  df$newmoonnumber <- newmoon_match_number[matches]
  df
}

#' @title Determine specific newmoon numbers
#'
#' @description 
#'  \code{target_newmoons}: based on the forecast origin and the lead time, 
#'  determines which moons should be included. \cr \cr
#'  \code{last_newmoon}: based on the forecast origin date, determine its
#'  newmoon number.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
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
#' @return 
#'  \code{target_newmoons}: \code{numeric} vector of the newmoon numbers 
#'  targeted by the date window.
#'  \code{last_newmoon}:  \code{numeric} value of the last newmoon number.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   target_newmoons()
#'   last_newmoon()
#'  }
#'
#' @export
#'
target_newmoons <- function(main = ".", moons = NULL,
                            end_moon = NULL, lead_time = 12, 
                            cast_date = Sys.Date(), arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  last_moon <- last_newmoon(main = main, moons = moons, cast_date = cast_date, 
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, 500)
  (end_moon + 1):(end_moon + lead_time)
}

#' @rdname target_newmoons
#'
#' @export
#'
last_newmoon <- function(main = ".", moons = NULL,
                           cast_date = Sys.Date(), arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  which_last_moon <- max(which(moons$newmoondate < cast_date))
  moons$newmoonnumber[which_last_moon]
}

