#' @title Prepare temporal (lunar) data
#'
#' @description Get time information (calendar dates, census periods, 
#'   newmoon numbers) associated with trapping events (achieved and missed)
#'   based on a lunar survey schedule. If needed, additional moons will be
#'   added to the data table, both the in-use and raw versions
#'
#' @param options_moons control options list for moons
#'
#' @return data table of time variables for all surveys
#'
#' @export
#' 
prep_moons <- function(options_moons = moons_options()){

  if (!options_moons$quiet){
    message("Loading the moons data file into the data subdirectory")
  }

  verify_PortalData(options_moons$tree, "moon_dates.csv")

  file_path(options_moons$tree, "PortalData/Rodents/moon_dates.csv") %>%
  read.csv(stringsAsFactors = FALSE) %>% 
  add_future_moons(options_moons) %>%
  append_past_moons_to_raw(options_moons) %>%
  format_moons() %>%
  dataout(options_moons)

}

#' @title Append missing past moons to the raw data
#'
#' @description Sometimes the raw moon data table is not up-to-date. Because
#'   the portalr functions \code{weather} and \code{fcast_ndvi} point to the
#'   raw moons data, that table needs to be updated to produce the correct
#'   current data table
#'
#' @param data moons table with future moons added
#'
#' @param options_moons moon data options
#'
#' @return data as came in
#'
#' @export
#'
append_past_moons_to_raw <- function(data, options_moons = moons_options()){
  if (options_moons$append_missing_to_raw){
    path <- file_path(options_moons$tree, "PortalData/Rodents/moon_dates.csv")
    included_moons <- data$newmoondate <= today()
    newraw <- data[included_moons, ]
    write.csv(newraw, path, row.names = FALSE)
  }
  data
}

#' @title Add future moons to the moon table
#'
#' @description Add future moons to the moon table, counting forward from
#'   \code{fdate}. Because the moon table might not have the most recent 
#'   moons, more rows than \code{n_future_moons} may be added to the table.
#'
#' @param moons current newmoonnumber table
#'
#' @param options_moons control options list for moons
#'
#' @return appended moon table
#'
#' @export
#' 
add_future_moons <- function(moons = prep_moons(), 
                             options_moons = moons_options()){

  n_future_moons <- options_moons$n_future_moons
  fdate <- options_moons$fdate
  if (!(n_future_moons %% 1 == 0 & n_future_moons >= 0)){
    stop("'n_future_moons' must be a non-negative integer")
  }
  if (n_future_moons == 0){
    return(moons)
  }
  if (!is.Date(fdate)){
    stop("'fdate' must be a date.")
  }
  get_future_moons(moons, n_future_moons) %>% 
  add_addl_future_moons(fdate = fdate) %>%
  mutate(newmoondate = as.character(newmoondate)) %>%
  bind_rows(moons, .)

}

#' @title Add more more moons to accomplish requested forecast length
#'
#' @description Add extra future moons to the moon table to achieve requested
#'   forecast length. Because the moon table might not have the most recent 
#'   moons, more rows than initially requested may need to be added to the 
#'   table.
#'
#' @param future_moons the table of moons forward from the moon table
#'
#' @param fdate date from which future is defined, typically today's date.
#'
#' @return appended moon table
#'
#' @export
#' 
add_addl_future_moons <- function(future_moons, fdate){

  n_addl_future_moons <- length(which(future_moons$newmoondate < fdate))
  if (n_addl_future_moons > 0){
    addl_moons <- get_future_moons(future_moons, n_addl_future_moons)
    future_moons <- bind_rows(future_moons, addl_moons)
  } 
  return(future_moons)
}

#' @title Format the moon data
#'
#' @description Add year and month columns and encode newmoon date as a Date
#'
#' @param moons newmoon table
#'
#' @return appended moon table
#'
#' @export
#' 
format_moons <- function(moons = prep_moons()){
  moons$year <- year(moons$newmoondate)
  moons$month <- month(moons$newmoondate)
  moons$newmoondate <- as.Date(moons$newmoondate)
  return(moons)
}
