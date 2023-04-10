#' @title Prepare Lunar Data for the Portalcasting Repository
#'
#' @description Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. 
#'              `add_forecast_newmoons` adds future newmoon dates to the newmoon table from `prepare_newmoons` associated with the forecast.
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param newmoons `data.frame` of newmoon data.
#'
#' @return Some version of a newmoons `data.frame`. \cr \cr
#'         `prepare_newmoons`: fully appended and formatted `data.frame` (also saved out if `settings$save = TRUE`). \cr \cr
#'         `add_forecast_newmoons`: fully appended and formatted `data.frame`. 
#'
#' @name prepare newmoons
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "newmoons")
#'
#'    create_dir(main = main1)
#'    fill_resources(main = main1)
#'    fill_forecasts(main = main1)
#'    fill_fits(main = main1)
#'    fill_models(main = main1)
#'
#'    prepare_newmoons(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL

#' @rdname prepare-newmoons
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
             overwrite = settings$overwrite, 
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


