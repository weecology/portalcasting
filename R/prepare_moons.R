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
#'   the \code{portalr} functions \code{\link[portalr]{weather}} and 
#'   \code{\link[portalr]{fcast_ndvi}} point to the raw moons data, that table
#'   needs to be updated to produce the correct current data table for 
#'   casting. \code{add_past_moons_to_raw} updates the raw file accordingly 
#'   \cr \cr
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param cast_date \code{Date} from which future is defined, typically 
#'   today's date (set using \code{\link{Sys.Date}}).
#'
#' @param moons Moons \code{data.frame}.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  raw moons file should be updated with the past moons.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#' 
#' @param raw_path \code{character} value indicating the file path
#'  to the data within \code{raw}. A standard portalcasting directory 
#'  downloads the raw data files into \code{"raw\PortalData"}, so 
#'  \code{raw_path = "PortalData/Rodents/moon_dates.csv"} (as \code{"raw/"} 
#'  is implied). 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @return All functions here return some version of a moons \code{data.frame}
#'  \cr \cr. 
#'  \code{prep_moons}, \code{format_moons}: fully appended and formatted 
#'  \code{data.frame}. \cr \cr
#'  \code{add_past_moons_to_raw}, \code{add_future_moons}, 
#'  \code{add_extra_future_moons}: appropropriately appended \code{moons} 
#'  data table as a \code{moons}-class \code{data.frame}.
#'   
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   prep_moons()
#'   fpath <- file_paths(".", "raw/PortalData/Rodents/moon_dates.csv")
#'   moons <- read.csv(fpath, stringsAsFactors = FALSE)
#'   moons <- add_future_moons(moons)
#'   moons <- add_past_moons_to_raw(moons)
#'   format_moons(moons)
#'  }
#'
#' @export
#' 
prep_moons <- function(lead_time = 12, cast_date = Sys.Date(), 
                       raw_path = "PortalData/Rodents/moon_dates.csv", 
                       main = ".", quiet = FALSE, overwrite = TRUE){
  paste0("raw/", raw_path) %>%
  file_paths(main, .) %>%
  read.csv(stringsAsFactors = FALSE) %>%
  add_future_moons(lead_time, cast_date) %>%
  add_past_moons_to_raw(main, raw_path, overwrite) %>%
  format_moons()
}

#' @rdname prep_moons
#'
#' @export
#'
format_moons <- function(moons){
  moons$year <- year(moons$newmoondate)
  moons$month <- month(moons$newmoondate)
  moons$newmoondate <- as.Date(moons$newmoondate)
  moons
}

#' @rdname prep_moons
#'
#' @export
#'
add_future_moons <- function(moons = prep_moons(), lead_time = 12, 
                             cast_date = Sys.Date()){
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
add_extra_future_moons <- function(moons, cast_date = Sys.Date()){
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
add_past_moons_to_raw <- function(moons, main = ".", raw_path = 
                                  "PortalData/Rodents/moon_dates.csv",
                                  overwrite = TRUE){
  if(overwrite){
    fpath <- file_paths(main, paste0("raw/", raw_path)) 
    included_moons <- moons$newmoondate < Sys.Date()
    new_raw <- moons[included_moons, ]
    write.csv(new_raw, fpath, row.names = FALSE)
  }
  moons
}