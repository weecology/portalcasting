#' @importFrom forecast auto.arima forecast na.interp
#' @importFrom httr content GET stop_for_status
#' @importFrom portalr download_observations get_future_moons load_trapping_data ndvi summarize_rodent_data weather
#' @importFrom utils download.file read.csv unzip
#' @importFrom yaml read_yaml write_yaml

#' @title Functions for Portalcasting (Forecasting Portal Rodents)
#'
#' @description Functions used for continuous analysis and forecasting of \href{https://portal.weecology.org/}{Portal rodent populations}, as implemented in the \href{https://bit.ly/2Lo2xnQ}{Portal Predictions repository}. \cr \cr
#'              The code in \code{portalcasting} is designed to be portable, allowing users to set up a fully-functional replica repository for development and testing of new models (a.k.a. \emph{sandboxing}) in the same directory and codebase as the production pipeline. 
#'
#' @name portalcasting
#'
#' @docType package
#'
#' @keywords package
#'
NULL

