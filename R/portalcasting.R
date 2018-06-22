#' @importFrom digest digest
#' @importFrom dplyr bind_rows filter full_join group_by inner_join left_join 
#'   right_join mutate rename select ungroup summarise summarize
#' @importFrom forecast auto.arima ets forecast na.interp
#' @importFrom httr content GET stop_for_status
#' @importFrom lubridate as_date is.Date month year
#' @importFrom magrittr %>% extract2
#' @importFrom portalr download_observations fcast_ndvi get_future_moons
#'   get_rodent_data ndvi weather
#' @importFrom purrr map
#' @importFrom RCurl getURL
#' @importFrom rlang !! !!! .data quo quos
#' @importFrom stats AIC qnorm predict
#' @importFrom tscount tsglm
#' @importFrom utils download.file read.csv read.table tail write.csv 
#'   write.table
#' @importFrom yaml as.yaml yaml.load_file
#'

#' @title Functions for Portal Forecasting
#'
#' @description This package contains the functions used to forecast rodent
#'   abundance at Portal.
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
    c(".", "aic", "censusdate", "currency", "date", "delta_aic", 
      "ensemble_estimate", "ensemble_var", 
      "estimate", "fit_end_newmoon", "fit_start_newmoon", 
      "forecast_newmoon", "forecastmonth", "forecastyear", "initial_newmoon",
      "level", "LowerPI", "maxtemp", "meantemp", "mintemp", "model_var", "n", 
      "newmoondate", "newmoonnumber", "period", "PI", "precipitation", 
      "species", "sum_weight", "treatment",
      "UpperPI", "weight", "weighted_ss")
  )
}