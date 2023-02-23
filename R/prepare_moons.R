#' @title Prepare Lunar Data for the Portalcasting Repository
#'
#' @description Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. If needed, additional moons will be added to both the in-use and resources versions of the data table. \cr \cr
#'              \code{add_future_newmoons} adds future newmoon dates to the newmoon table, counting forward from \code{origin}. Because the \code{newmoons} table might not have the most recent newmoons, more rows than \code{lead} may need to be added to the table. \cr \cr. 
#'              \code{get_cast_future_newmoons} wraps around \code{\link[portalr]{get_future_newmoons}} to include any additional newmoons needed to achieve the full \code{lead} from \code{origin}. \cr \cr
#'
#' @details Sometimes the resources newmoon data table is not fully up-to-date. Because the \code{portalr} functions \code{\link[portalr]{weather}} and \code{\link[portalr]{fcast_ndvi}} point to the resources newmoons data, that table needs to be updated to produce the correct current data table for casting. 
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param moons \code{data.frame} of moon data.
#'
#' @param origin \code{Date} forecast origin, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @param lead \code{integer} (or integer \code{numeric}) value for the number of days forward a cast will cover.
#'
#' @return Some version of a newmoons \code{data.frame}. \cr \cr. 
#'         \code{prepare_newmoons}: fully appended and formatted \code{data.frame} (also saved out if \code{settings$save = TRUE}). \cr 
#'         \code{add_future_newmoons}: fully appended and formatted \code{data.frame}. \cr 
#'         \code{forecast_future_newmoons}: newmoons \code{data.frame} to append to the existing \code{moons}.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   prepare_newmoons()
#'  }
#'
#' @name prepare newmoons
#'
#' @export
#' 
prepare_newmoons <- function (main     = ".", 
                              lead     = 365,
                              origin   = Sys.Date(), 
                              settings = directory_settings(), 
                              quiet    = TRUE,
                              verbose  = FALSE) {

  
  messageq("  - newmoons data file", quiet = quiet)

  traps_in <- load_trapping_data(path                = file.path(main, settings$subdirectories["resources"]), 
                                 download_if_missing = FALSE,
                                 clean               = FALSE, 
                                 quiet               = !verbose)

  newmoons_in <- traps_in[["newmoons_table"]]

  newmoons_out <- add_future_newmoons(main      = main, 
                                      newmoons  = newmoons_in, 
                                      lead      = lead, 
                                      origin    = origin)

  write_data(x         = newmoons_out, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$newmoons, 
             overwrite = settings$overwrite, 
             quiet     = !verbose)

} 

#' @rdname prepare-newmoons
#'
#' @export
#'
add_future_newmoons <- function (main      = ".", 
                                 newmoons  = NULL, 
                                 lead      = 365, 
                                 origin    = Sys.Date(), 
                                 settings  = directory_settings()) {

  return_if_null(newmoons)

  if (lead == 0) {

    return(newmoons)

  }
 
  rbind(newmoons, 
        forecast_future_newmoons(newmoons  = newmoons, 
                                 lead      = lead, 
                                 origin    = origin))
 
}

get_future_newmoons <- function (newmoons, num_future_newmoons) {

  get_future_moons(moons            = newmoons,
                   num_future_moons = num_future_newmoons)

}

#' @rdname prepare-newmoons
#'
#' @export
#'
forecast_future_newmoons <- function (newmoons  = NULL, 
                                      lead   = 365, 
                                      origin = Sys.Date()) {

  return_if_null(newmoons)

  num_future_newmoons <- floor(lead / mean(as.numeric(diff(as.Date(newmoons$newmoondate))), na.rm = TRUE))

  future_newmoons <- get_future_newmoons(newmoons            = newmoons, 
                                         num_future_newmoons = num_future_newmoons)


  n_extra_future_newmoons <- length(which(future_newmoons$newmoondate < origin))

  if (n_extra_future_newmoons > 0){

    extra_newmoons  <- get_future_newmoons(newmoons            = future_newmoons, 
                                           num_future_newmoons = n_extra_future_newmoons)
    future_newmoons <- rbind(future_newmoons, extra_newmoons)

  } 

  future_newmoons$newmoondate <- as.character(future_newmoons$newmoondate)

  future_newmoons

}

