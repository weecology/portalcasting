#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param directory_config_file \code{character} value of the path to the directory config YAML.
#'
#' @param moons_file \code{character} name of the file for saving the moons data.
#'
#' @param covariates_file,historical_covariates_file,forecast_covariates_file \code{character} name of the files for saving the covariate data (combined historical and forecast) and each component.
#'
#' @param metadata_file \code{character} name of the file for saving the forecast metadata.
#'
#' @param dataset_controls_file \code{character} name of the file for saving the rodent dataset controls.
#'
#' @param model_controls_file \code{character} name of the file for saving the model controls.
#'
#' @param subdirectories \code{character} vector of the subdirectory names. Default includes \code{resources}, \code{data}, \code{models}, and \code{casts}. 
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
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing files should be updated (most users should leave as \code{TRUE}).
#'
#' @param unzip_pause Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delayment. 
#'
#' @param download_timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @return Named \code{list} of settings for the directory.
#'
#' @export
#'
directory_settings <- function (directory_config_file      = "dir_config.yaml",
                                moons_file                 = "moon_dates.csv",
                                covariates_file            = "covariates.csv",
                                historical_covariates_file = "historical_covariates.csv",
                                forecast_covariates_file   = "forecast_covariates.csv",
                                dataset_controls_file      = "data/dataset_controls.yaml",
                                model_controls_file        = "models/model_controls.yaml",
                                metadata_file              = "metadata.yaml",
                                subdirectories             = list("forecasts" = "casts", "models" = "models", "resources" = "raw", "data" = "data"),
                                PortalData                 = list(source = "github", version = "latest"),
                                portalPredictions          = list(source = "github", version = NULL),
                                climate_forecast           = list(source = "NMME", version = as.character(Sys.Date()), data = c("tasmin", "tasmean", "tasmax", "pr")),
                                save                       = TRUE,
                                overwrite                  = TRUE, 
                                unzip_pause                = 30,
                                download_timeout           = getOption("timeout")) {

  list(files            = list(directory_config      = directory_config_file,
                               moons                 = moons_file,
                               covariates            = covariates_file,
                               historical_covariates = historical_covariates_file,
                               forecast_covariates   = forecast_covariates_file,
                               dataset_controls      = dataset_controls_file,
                               model_controls        = model_controls_file,
                               metadata              = metadata_file),
       subs             = subdirectories,
       resources        = list(PortalData            = PortalData,
                               portalPredictions     = portalPredictions,
                               climate_forecast      = climate_forecast),
       repository       = "portalPredictions",
       save             = save, 
       overwrite        = overwrite, 
       unzip_pause      = 30,
       download_timeout = download_timeout)

}

