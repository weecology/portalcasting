
#' @importFrom forecast na.interp
#' @importFrom httr content GET stop_for_status
#' @importFrom portalr download_observations normalized_file_path 
#'  summarize_rodent_data weather
#' @importFrom tools file_ext
#' @importFrom utils download.file packageDescription read.csv sessionInfo
#'  unzip write.csv write.table
#' @importFrom yaml yaml.load_file write_yaml

#' @title Functions for Portalcasting (Forecasting Portal Rodents)
#'
#' @description This package contains the functions used for continuous
#'              analysis and forecasting of 
#'  \href{https://portal.weecology.org/}{Portal rodent populations},
#'              as implemented in the 
#'  \href{https://bit.ly/2Lo2xnQ}{Portal Predictions repository}. 
#'              \cr \cr
#'              Functions in \code{portalcasting} are designed to be portable,
#'              allowing users to set up a fully-functional replica 
#'              repository for development and testing of new models (a.k.a. 
#'              \emph{sandboxing}) in the same directory and codebase as the 
#'              production pipeline. 
#'
#' @name portalcasting
#'
#' @docType package
#'
#' @keywords package
#'
NULL

