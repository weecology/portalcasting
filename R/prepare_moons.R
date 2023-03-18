#' @title Prepare Lunar Data for the Portalcasting Repository
#'
#' @description Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. 
#'              \code{add_forecast_newmoons} adds future newmoon dates to the newmoon table from \code{prepare_newmoons} associated with the forecast.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
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
prepare_newmoons <- function (main     = ".",
                              settings = directory_settings( ), 
                              quiet    = FALSE, 
                              verbose  = FALSE) {
  
  messageq("  - newmoons", quiet = quiet)

  newmoons <- load_datafile(datafile            = file.path("Rodents", "moon_dates.csv"),
                            path                = file.path(main, settings$subdirectories$resources),
                            download_if_missing = FALSE,
                            na.strings          = "NA",
                            quiet               = !verbose)
  newmoons <- add_forecast_newmoons(main        = main, 
                                    settings    = settings,
                                    newmoons    = newmoons)

  newmoons <- newmoons[newmoons$newmoondate >= settings$time$timeseries_start_lagged, ]

  write_data(x         = newmoons, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$newmoons, 
             quiet     = !verbose)

} 

#' @rdname prepare-newmoons
#'
#' @export
#'
add_forecast_newmoons <- function (main      = ".", 
                                   newmoons  = NULL, 
                                   settings  = directory_settings( )) {

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

get_future_newmoons <- function (newmoons, nfuture_newmoons) {

  get_future_moons(moons            = newmoons,
                   num_future_moons = nfuture_newmoons)
# will be available directly in portalr in place of get future moons shortly
}


