#' @importFrom curl curl
#' @importFrom DesignLibrary match.call.defaults
#' @importFrom dplyr %>% arrange bind_rows filter group_by inner_join mutate 
#'  one_of right_join select summarize ungroup
#' @importFrom httr content GET stop_for_status
#' @importFrom lubridate month year
#' @importFrom portalr fcast_ndvi get_future_moons ndvi summarize_rodent_data 
#'  weather
#' @importFrom readr read_csv
#' @importFrom rlang !! .data
#' @importFrom stats rnorm
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils download.file packageDescription read.csv tail unzip
#'  write.csv

#' @title Functions for Portal Forecasting
#'
#' @description This package contains the functions used for continuous
#'   analysis and forecasting of 
#'   \href{https://portal.weecology.org/}{Portal rodent populations},
#'   as implemented in the 
#'   \href{https://bit.ly/2Lo2xnQ}{Portal Predictions repository}.
#'   Functions in \code{portalcasting} are designed to be portable, allowing
#'   users to set up a fully-functional replica repository for development
#'   and testing of new models. 
#'
#' @name portalcasting
#'
#' @docType package
#'
#' @keywords package
#'
NULL

# To quiet concerns of R CMD check re: variables used in non-standard eval
if (getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(".", "battery_low", "censusdate", "day", "date_made", 
      "forecast_newmoon", "locally_measured", "main", "maxtemp", "meantemp", 
      "mintemp", "newmoondate", "newmoonnumber", "precipitation")
  )
}

