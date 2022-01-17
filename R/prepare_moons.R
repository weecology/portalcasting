#' @title Prepare Lunar Data for the Portalcasting Repository
#'
#' @description Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. If needed, additional moons will be added to both the in-use and resources versions of the data table. \cr \cr
#'              \code{add_future_moons} adds future moon dates to the moon table, counting forward from \code{cast_date}. Because the \code{moons} table might not have the most recent moons, more rows than \code{lead} may need to be added to the table. \cr \cr. 
#'              \code{get_cast_future_moons} wraps around \code{\link[portalr]{get_future_moons}} to include any additional moons needed to achieve the full \code{lead} from \code{cast_date}. \cr \cr
#'
#' @details Sometimes the resources moon data table is not fully up-to-date. Because the \code{portalr} functions \code{\link[portalr]{weather}} and \code{\link[portalr]{fcast_ndvi}} point to the resources moons data, that table needs to be updated to produce the correct current data table for casting. 
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
#' @param lead \code{integer} (or integer \code{numeric}) value for the number of days forward a cast will cover.
#'
#' @return Some version of a moons \code{data.frame}. \cr \cr. 
#'         \code{prepare_moons}: fully appended and formatted \code{data.frame} (also saved out if \code{save = TRUE}). \cr 
#'         \code{add_future_moons}: fully appended and formatted \code{data.frame}. \cr 
#'         \code{forecast_future_moons}: moons \code{data.frame} to append to the existing \code{moons}.
#'
#' @name prepare_moons
#'
NULL

#' @rdname prepare_moons
#'
#' @export
#' 
prepare_moons <- function (main     = ".", 
                           lead     = 365,
                           origin   = Sys.Date(), 
                           settings = directory_settings(), 
                           quiet    = TRUE,
                           verbose  = FALSE) {

  
  PD <- file.path(main, "resources", "PortalData")
  
  if (!file.exists(PD)) {

    download_observations(path        = file.path(main, "resources"), 
                          version     = settings$resources$PortalData$version,
                          from_zenodo = settings$resources$PortalData$source == "zenodo",
                          quiet       = quiet)

  }

  messageq("  - moons data file", quiet = quiet)

  traps_in <- load_trapping_data(path                = file.path(main, "resources"), 
                                 download_if_missing = FALSE,
                                 clean               = FALSE, 
                                 quiet               = !verbose)

  moons_in <- traps_in[["newmoons_table"]]

  moons_out <- add_future_moons(main   = main, 
                                moons  = moons_in, 
                                lead   = lead, 
                                origin = origin)

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
                              lead = 365, 
                              origin    = Sys.Date(), 
                              settings  = directory_settings()) {

  return_if_null(moons)

  if (lead == 0) {

    return(moons)

  }
 
  rbind(moons, 
        forecast_future_moons(moons  = moons, 
                              lead   = lead, 
                              origin = origin))

}

#' @rdname prepare_moons
#'
#' @export
#'
forecast_future_moons <- function (moons  = NULL, 
                                   lead   = 365, 
                                   origin = Sys.Date()) {

  return_if_null(moons)

  num_future_moons <- floor(lead / mean(as.numeric(diff(as.Date(moons$newmoondate))), na.rm = TRUE))

  future_moons <- get_future_moons(moons            = moons, 
                                   num_future_moons = num_future_moons)


  n_extra_future_moons <- length(which(future_moons$newmoondate < origin))

  if (n_extra_future_moons > 0){

    extra_moons  <- get_future_moons(moons            = future_moons, 
                                     num_future_moons = n_extra_future_moons)
    future_moons <- rbind(future_moons, extra_moons)

  } 

  future_moons$newmoondate <- as.character(future_moons$newmoondate)

  future_moons

}

