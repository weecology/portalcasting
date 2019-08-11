#' @importFrom DesignLibrary match.call.defaults
#' @importFrom dplyr %>% bind_rows filter inner_join mutate one_of select
#' @importFrom httr content GET stop_for_status
#' @importFrom lubridate month year
#' @importFrom portalr get_future_moons summarize_rodent_data
#' @importFrom rlang !!
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils download.file packageDescription read.csv unzip
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
    c(".", "censusdate", "newmoondate", "newmoonnumber")
  )
}

