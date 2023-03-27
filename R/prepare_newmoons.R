#' @title Prepare Lunar Data for the Portalcasting Repository
#'
#' @description Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. 
#'              \code{add_forecast_newmoons} adds future newmoon dates to the newmoon table from \code{prepare_newmoons} associated with the forecast.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param newmoons \code{data.frame} of newmoon data.
#'
#' @return Some version of a newmoons \code{data.frame}. \cr \cr. 
#'         \code{prepare_newmoons}: fully appended and formatted \code{data.frame} (also saved out if \code{settings$save = TRUE}). \cr 
#'         \code{add_forecast_newmoons}: fully appended and formatted \code{data.frame}. \cr 
#'
#' @name prepare newmoons
#'
#' @export
#' 
prepare_newmoons <- function (main = ".") {
  
  settings <- read_directory_settings(main = main)

  messageq("  - newmoons", quiet = settings$quiet)

  newmoons <- load_datafile(datafile            = file.path("Rodents", "moon_dates.csv"),
                            path                = file.path(main, settings$subdirectories$resources),
                            download_if_missing = FALSE,
                            na.strings          = "NA",
                            quiet               = !settings$verbose)
  newmoons <- add_forecast_newmoons(main        = main,
                                    newmoons    = newmoons)

  ### patch to verify the correct moons are in ###
    nlead_newmoons   <- sum(newmoons$newmoondate >= settings$time$origin)
    nneeded_newmoons <- ceiling(settings$time$lead_time / 29.5)
    nshort           <- nneeded_newmoons - nlead_newmoons
    if (nshort > 0) {
      addl_newmoons    <- get_future_newmoons(newmoons         = newmoons,
                                              nfuture_newmoons = nshort)
      addl_newmoons$newmoondate <- as.character(addl_newmoons$newmoondate)
      newmoons         <- rbind(newmoons,
                                addl_newmoons)
    }
  ### end patch ###

# dont cut them out of the actual data tho...
#  newmoons <- newmoons[newmoons$newmoondate >= settings$time$timeseries_start_lagged & 
#                       newmoons$newmoondate <  settings$time$forecast_end_buffered, ]

  write_data(x         = newmoons, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$newmoons, 
             quiet     = !settings$verbose)

} 

#' @rdname prepare-newmoons
#'
#' @export
#'
add_forecast_newmoons <- function (main     = ".",
                                   newmoons = NULL) {

  settings <- read_directory_settings(main = main)

  return_if_null(newmoons)

  if (settings$time$lead_time == 0) {

    return(newmoons)

  }
 
  nforecast_newmoons <- ceiling(settings$time$lead_time / 29.5)
  forecast_newmoons  <- get_future_newmoons(newmoons         = newmoons, 
                                            nfuture_newmoons = nforecast_newmoons)

  forecast_newmoons$newmoondate <- as.character(forecast_newmoons$newmoondate)
  rbind(newmoons, 
        forecast_newmoons)
 
}


