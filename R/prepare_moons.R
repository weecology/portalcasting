#' @title Prepare Lunar Data for the Portalcasting Repository
#'
#' @description Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. If needed, additional moons will be added to both the in-use and raw versions of the data table. \cr \cr
#'              \code{add_future_moons} adds future moon dates to the moon table, counting forward from \code{cast_date}. Because the \code{moons} table might not have the most recent moons, more rows than \code{lead_time} may need to be added to the table. \cr \cr. 
#'              \code{get_cast_future_moons} wraps around \code{\link[portalr]{get_future_moons}} to include any additional moons needed to achieve the full \code{lead_time} from \code{cast_date}. \cr \cr
#'
#' @details Sometimes the raw moon data table is not fully up-to-date. Because the \code{portalr} functions \code{\link[portalr]{weather}} and \code{\link[portalr]{fcast_ndvi}} point to the raw moons data, that table needs to be updated to produce the correct current data table for casting. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param origin \code{Date} forecast origin, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the number of timesteps forward a cast will cover.
#'
#' @return Some version of a moons \code{data.frame}. \cr \cr. 
#'         \code{prepare_moons}: fully appended and formatted \code{data.frame} (also saved out if \code{save = TRUE}). \cr 
#'         \code{add_future_moons} and \code{add_extra_future_moons}: appropriately appended moons \code{data.frame}.
#'   
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   prepare_moons()
#'  }
#'
#' @name prepare_moons
#'
NULL

#' @rdname prepare_moons
#'
#' @export
#' 
prepare_moons <- function (main      = ".", 
                           lead_time = 12,
                           origin    = Sys.Date(), 
                           settings  = directory_settings(), 
                           quiet     = TRUE,
                           verbose   = FALSE) {

  
  PD <- file.path(main, "raw", "PortalData")
  
  if (!file.exists(PD)) {

    download_observations(path        = file.path(main, "raw"), 
                          version     = settings$resources$PortalData$version,
                          from_zenodo = settings$resources$PortalData$source == "zenodo",
                          quiet       = quiet)

  }

  messageq(" - moons data file", quiet = quiet)

  traps_in <- load_trapping_data(path                = file.path(main, "raw"), 
                                 download_if_missing = FALSE,
                                 clean               = FALSE, 
                                 quiet               = !verbose)

  moons_in <- traps_in[["newmoons_table"]]

  moons_out <- add_future_moons(main      = main, 
                                moons     = moons_in, 
                                lead_time = lead_time, 
                                origin    = origin)

  write_data(dfl       = moons_out, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$moons, 
             overwrite = settings$overwrite, 
             quiet     = !verbose)

} 

#' @rdname prepare_moons
#'
#' @export
#'
add_future_moons <- function (main      = ".", 
                              moons     = NULL, 
                              lead_time = 12, 
                              origin    = Sys.Date(), 
                              settings  = directory_settings()) {

  return_if_null(moons)

  if (lead_time == 0) {

    return(moons)

  }

  future_moons <- get_forecast_future_moons(moons     = moons, 
                                            lead_time = lead_time,
                                            origin    = origin)

  future_moons$newmoondate <- as.character(future_moons$newmoondate)
  rbind(moons, future_moons)

}

#' @rdname prepare_moons
#'
#' @export
#'
get_forecast_future_moons <- function (moons     = NULL, 
                                       lead_time = 12, 
                                       origin    = Sys.Date()) {

  return_if_null(moons)

  future_moons <- get_future_moons(moons            = moons, 
                                   num_future_moons = lead_time)


  n_extra_future_moons <- length(which(future_moons$newmoondate < origin))

  if (n_extra_future_moons > 0){

    extra_moons  <- get_future_moons(moons            = future_moons, 
                                     num_future_moons = n_extra_future_moons)
    future_moons <- rbind(future_moons, extra_moons)

  } 

  future_moons

}

#' @title Add a Newmoon Number Column to a Table that has a Date Column 
#' 
#' @description Add a \code{newmoonnumber} column to a table that has a \code{date} column.
#' 
#' @param df \code{data.frame} with column of \code{date}s.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @return \code{data.frame} \code{df} with column of \code{newmoonnumber}s added.
#'
#' @export
#'
add_newmoonnumbers_from_dates <- function (df, moons = NULL) {

  return_if_null(moons, df)

  if (is.null(df$date)) {

    df <- add_date_from_components(df)
  }

  moon_number       <- moons$newmoonnumber[-1]
  moon_start        <- as.Date(moons$newmoondate[-nrow(moons)])
  moon_end          <- as.Date(moons$newmoondate[-1])
  moon_match_number <- NULL
  moon_match_date   <- NULL

  for (i in seq(moon_number)) {

    temp_dates        <- seq.Date(moon_start[i] + 1, moon_end[i], 1)
    temp_dates        <- as.character(temp_dates)
    temp_numbers      <- rep(moon_number[i], length(temp_dates))
    moon_match_date   <- c(moon_match_date, temp_dates)
    moon_match_number <- c(moon_match_number, temp_numbers)

  }

  moon_match_date  <- as.Date(moon_match_date)
  moon_matches     <- match(df$date, moon_match_date)
  df$newmoonnumber <- moon_match_number[moon_matches]

  df

}


