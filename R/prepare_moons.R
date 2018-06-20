#' @title Prepare temporal (lunar) data
#'
#' @description Get time information (calendar dates, census periods, 
#'   newmoon numbers) associated with trapping events (achieved and missed)
#'   based on a lunar survey schedule. If needed, additional moons will be
#'   added to the data table, both the in-use and raw versions
#'
#' @param tree directory tree
#'
#' @param data_options control options list for moons
#'
#' @return data table of time variables for all surveys
#'
#' @export
#' 
prep_moons <- function(tree = dirtree(), data_options = moons_options()){

  verify_PortalData(tree = tree, "moon_dates.csv")

  file_path(tree = tree, "PortalData/Rodents/moon_dates.csv") %>%
  read.csv(stringsAsFactors = FALSE) %>% 
  add_future_moons(data_options = data_options) %>%
  append_past_moons_to_raw(tree = tree, data_options = data_options) %>%
  format_moons() %>%
  dataout(tree, data_options)

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
#' @param tree directory tree
#'
#' @param data_options moon data options
#'
#' @return data as came in
#'
#' @export
#'
append_past_moons_to_raw <- function(data, tree = dirtree(), data_options){
  if (data_options$append_missing_to_raw){
    path <- file_path(tree = tree, "PortalData/Rodents/moon_dates.csv")
    included_moons <- data$newmoondate <= today()
    newraw <- data[included_moons, ]
    write.csv(newraw, path, row.names = FALSE)
  }
  data
}


#' @title Prepare the data options for moon data
#'
#' @description Create a list of control options for the moon data
#'
#' @param n_future_moons integer value for the number of future moons to add
#'
#' @param fdate date from which future is defined, typically today's date.
#'   Required if \code{n_future_moons > 0}
#'
#' @param append_missing_to_raw logical indicating if the raw data should be
#'   updated by appending the missing moon dates
#'
#' @param save logical if the data should be saved out
#'
#' @param filename the name of the file for the saving.
#'
#' @return control options list for moons
#'
#' @export
#'
moons_options <- function(n_future_moons = 12, fdate = today(), 
                          append_missing_to_raw = TRUE, save = TRUE,
                          filename = "moons.csv"){
  list("n_future_moons" = n_future_moons, "fdate" = fdate, 
       "append_missing_to_raw" = append_missing_to_raw, "save" = save,
       "filename" = filename)
}

#' @title Verify that the PortalData sub is present and has required data
#'
#' @description Check that the PortalData subdirectory exists and has the
#'   needed file. If the file is not present, the directory is filled.
#'
#' @param tree directory tree
#'
#' @param filename name of the file to specifically check
#'
#' @return nothing
#'
#' @export
#' 
verify_PortalData <- function(tree = dirtree(), filename = "moon_dates.csv"){
  path <- file_path(tree = tree, paste0("PortalData/Rodents/", filename)) 
  if (!file.exists(path)){
    create_dir(tree = tree)
    fill_PortalData(tree = tree)
  }
}

#' @title Add future moons to the moon table
#'
#' @description Add future moons to the moon table, counting forward from
#'   \code{fdate}. Because the moon table might not have the most recent 
#'   moons, more rows than \code{n_future_moons} may be added to the table.
#'
#' @param moons current newmoonnumber table
#'
#' @param data_options control options list for moons
#'
#' @return appended moon table
#'
#' @export
#' 
add_future_moons <- function(moons = prep_moons(), 
                             data_options = moons_options()){

  n_future_moons <- data_options$n_future_moons
  fdate <- data_options$fdate
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
