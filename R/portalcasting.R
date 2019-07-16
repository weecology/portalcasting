#' @importFrom curl curl new_handle
#' @importFrom DesignLibrary match.call.defaults
#' @importFrom digest digest
#' @importFrom dplyr arrange bind_rows filter full_join group_by inner_join 
#'   left_join mutate n rename right_join select summarise summarize ungroup 
#' @importFrom forecast auto.arima ets forecast na.interp
#' @importFrom graphics abline axis mtext par plot points polygon rect text
#' @importFrom grDevices rgb
#' @importFrom httr authenticate content headers http_type GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_date is.Date month year
#' @importFrom magrittr %>% extract2
#' @importFrom portalr download_observations fcast_ndvi 
#'   find_incomplete_censuses get_future_moons ndvi summarize_rodent_data 
#'   weather
#' @importFrom purrr map
#' @importFrom RCurl basicTextGatherer getURL curlOptions curlPerform 
#'   getCurlHandle
#' @importFrom readr read_csv
#' @importFrom rlang !! !!! .data quo quos
#' @importFrom stats AIC lm na.omit predict qnorm quantile runif setNames
#' @importFrom tidyselect one_of
#' @importFrom tscount tsglm
#' @importFrom usethis use_data
#' @importFrom utils download.file packageDescription read.csv read.table 
#'   tail unzip write.csv write.table
#' @importFrom viridis viridis
#' @importFrom yaml as.yaml yaml.load_file
#'

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
    c(".", "aic", "censusdate", "currency", "date", "date_made", "delta_aic", 
      "ensemble_estimate", "ensemble_var", 
      "estimate", "fit_end_newmoon", "fit_start_newmoon", 
      "forecast_newmoon", "forecastmonth", "forecastyear", "initial_newmoon",
      "level", "LowerPI", "maxtemp", "meantemp", "mintemp", "model_var",  
      "newmoondate", "newmoonnumber", "period", "PI", "precipitation", 
      "species", "sum_weight", "treatment", "locally_measured", "battery_low",
      "UpperPI", "weight", "weighted_ss", ".mapUnicode")
  )
}