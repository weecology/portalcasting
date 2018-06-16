#' @title Prepare temporal (lunar) data
#'
#' @description Get time information (calendar dates, census periods, 
#'   newmoon numbers) associated with trapping events (achieved and missed)
#'   based on a lunar survey schedule.
#'
#' @param future_moons integer value for the number of future moons to add
#'
#' @param fdate date from which future is defined, typically today's date.
#'   Required if \code{future > 0}
#'
#' @param data_dir directory name where the data files will reside
#'
#' @return data table of time variables for all surveys
#'
#' @export
#' 
prep_moons <- function(tree = dirtree(), n_future_moons = 0, fdate = today()){

  path <- file_path(tree = tree, "PortalData/Rodents/moon_dates.csv") 
  if (!file.exists(path)){
    create_dir(tree = tree)
    fill_PortalData(tree = tree)
  }
  out <- read.csv(path, stringsAsFactors = FALSE) %>% 
         add_future_moons(n_future_moons = n_future_moons, fdate = fdate) %>%
         format_moons() 
  path <- file_path(tree = tree, "data/moons.csv")
  write.csv(out, path, row.names = FALSE)
  return(out)
}

#' @title Add future moons to the moon table
#'
#' @description Add future moons to the moon table, counting forward from
#'   \code{fdate}. Because the moon table might not have the most recent 
#'   moons, more rows than \code{n_future_moons} may be added to the table.
#'
#' @param moons current newmoonnumber table
#'
#' @param n_future_moons integer value for the number of future moons to add
#'
#' @param fdate date from which future is defined, typically today's date.
#'   Required if \code{n_future_moons > 0}
#'
#' @return appended moon table
#'
#' @export
#' 
add_future_moons <- function(moons = prep_moons(), n_future_moons = 12, 
                             fdate = today()){

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
    addl_moons <- get_future_moons(future_moons, n_addl)
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
