#' @importFrom dplyr bind_rows filter full_join group_by inner_join left_join 
#'   right_join mutate rename select ungroup summarise summarize
#' @importFrom lubridate as_date is.Date month year
#' @importFrom magrittr %>%
#' @importFrom portalr abundance download_observations fcast_ndvi 
#'   get_future_moons ndvi weather
#' @importFrom purrr map
#' @importFrom RCurl getURL
#' @importFrom rlang !! !!! .data quo quos
#' @importFrom stats qnorm
#' @importFrom utils read.csv read.table tail write.table
#' @importFrom yaml as.yaml
#'

#' @title Functions for Portal Forecasting
#'
#' @description This package is contains the functions use to forecast rodent
#'   abundance at Portal.
#'
#' @name portalcasting
#' @docType package
#' @keywords package
#'
NULL