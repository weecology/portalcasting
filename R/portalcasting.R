#' @importFrom curl curl
#' @importFrom DesignLibrary match.call.defaults
#' @importFrom dplyr %>% arrange bind_rows filter group_by inner_join 
#'  left_join mutate n one_of rename right_join select summarize ungroup
#' @importFrom forecast Arima auto.arima ets forecast na.interp
#' @importFrom graphics abline axis mtext par plot points polygon rect text
#' @importFrom grDevices rgb
#' @importFrom httr content GET stop_for_status
#' @importFrom lubridate month year
#' @importFrom portalr find_incomplete_censuses get_future_moons 
#'  load_trapping_data ndvi summarize_rodent_data weather
#' @importFrom readr read_csv
#' @importFrom rlang !! !!! .data quo quos
#' @importFrom stats AIC lm na.omit predict qnorm quantile rnorm runif
#' @importFrom tscount tsglm
#' @importFrom utils download.file packageDescription read.csv tail unzip
#'  write.csv write.table
#' @importFrom viridis viridis
#' @importFrom yaml as.yaml yaml.load_file
#'

#' @title Functions for Portal -casting (Forecasting and Hindcasting)
#'
#' @description This package contains the functions used for continuous
#'  analysis and forecasting of 
#'  \href{https://portal.weecology.org/}{Portal rodent populations},
#'  as implemented in the 
#'  \href{https://bit.ly/2Lo2xnQ}{Portal Predictions repository}. \cr
#'  Functions in \code{portalcasting} are designed to be portable, allowing
#'  users to set up a fully-functional replica repository for development
#'  and testing of new models (a.k.a. \emph{sandboxing}) in the same 
#'  directory and codebase as the production pipeline. 
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
    c(".", "aic", "battery_low", "cast_moon", "castmonth", "castyear",
      "censusdate", "currency", "day", "date_made", "delta_aic", 
      "ensemble_estimate", "ensemble_var", "estimate",
      "fit_end_moon", "fit_start_moon",
      "forecast_moon", "initial_moon", "level",
      "locally_measured", "LowerPI", "main", "maxtemp", "meantemp", 
      "mintemp", "model_var", "moondate", "moon", "precipitation",
      "species", "sum_weight", "UpperPI", "weight", "weighted_ss")
  )
}

