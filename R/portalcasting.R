#' @importFrom dplyr bind_rows filter full_join group_by inner_join right_join 
#'   mutate select ungroup summarize
#' @importFrom lubridate as_date is.Date month year
#' @importFrom magrittr %>%
#' @importFrom portalr abundance download_observations fcast_ndvi 
#'   get_future_moons ndvi weather
#' @importFrom RCurl getURL
#' @importFrom rlang !! !!!
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