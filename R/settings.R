#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param directory_config_file \code{character} value of the path to the directory config YAML.
#'
#' @param subdirectories \code{character} vector of the subdirectory names. Default includes \code{tmp}, \code{raw}, \code{data}, \code{models}, \code{fits}, and \code{casts}. 
#'
#' @param PortalData \code{list} with \code{source} and \code{version} elements that are \code{character} values for the source and version of the Portal Data to download. Default values retrieve the latest data from github. \cr \cr
#'                   See \code{\link[portalr]{download_observations}}.
#'
#' @param portalPredictions \code{list} with \code{source} and \code{version} elements that are \code{character} values for the source and version of the archive to download. Default values point to github, but \code{version = NULL} indicates no download. \cr \cr 
#'                          See \code{\link{download_archive}}.
#'
#' @param climate_forecast \code{list} with \code{source} and \code{version} elements that are \code{character} values for the source and version of the climate forecasts to download. Default values retrieve the current day's forecast from the Northwest Knowledge Network's North American Multi-Model Ensemble (NMME) climate forecasts. \cr \cr 
#'                         See \code{\link{download_climate_forecasts}}.
#'
#' @return Named \code{list} of settings for the directory.
#'
#' @export
#'
directory_settings <- function (directory_config_file = "dir_config.yaml",
                                subdirectories        = c("casts", "fits", "models", "raw", "data", "tmp"),
                                PortalData            = list(source = "gitub", version = "latest"),
                                portalPredictions     = list(source = "gitub", version = NULL),
                                climate_forecast      = list("NMME", version = Sys.Date())){

  list(

    files     = list(
                  directory_config = directory_config_file),

    subs      = subdirectories,

    resources = list(
                  PortalData        = PortalData,
                  portalPredictions = portalPredictions,
                  climate_forecast  = climate_forecast) 

  )

}

