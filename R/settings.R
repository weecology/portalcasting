#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param files \code{list} of \code{character} names of standard files, see \code{\link{directory_files}}.
#'
#' @param subdirectories \code{list} of \code{character} names of standard subdirectories, see \code{\link{directory_subdirectories}}.
#'
#' @param resources \code{list} of \code{list}s for standard resources, see \code{\link{directory_resources}}.
#'
#' @param directory_configuration \code{character} name for the directory configuration YAML.
#'
#' @param moons \code{character} name for the lunar data csv.
#'
#' @param covariates \code{character} name for the combined historical and forecast covariates csv.
#'
#' @param historical_covariates \code{character} name for the historical covariates csv.
#'
#' @param forecast_covariates \code{character} name for the forecast covariates csv.
#'
#' @param forecast_metadata \code{character} name for the forecast metadata csv.
#'
#' @param metadata \code{character} name for the Forecast metadata YAML.
#'
#' @param dataset_controls \code{character} name for the YAML of dataset control list(s).
#'
#' @param model_controls \code{character} name for the YAML of model controls list(s).
#'
#' @param cast_evaluations \code{character} name for the forecast evaluations csv.
#'
#' @param resources \code{character} name for the resources subdirectory.
#'
#' @param data \code{character} name for the data subdirectory.
#'
#' @param models \code{character} name for the models subdirectory.
#'
#' @param fits \code{character} name for the fits subdirectory.
#'
#' @param forecasts \code{character} name for the forecasts subdirectory.
#'
#' @param PortalData \code{list} of \code{source} and \code{version} elements of \code{character} values for the Portal Data download. Default values retrieve the latest data from github
#'
#' @param portalPredictions \code{list} of \code{source} and \code{version} elements of \code{character} values for the archive download. Default values point to github, but \code{version = NULL} indicates no download.
#'
#' @param climate_forecast \code{list} of \code{source}, \code{version}, and \code{data} elements of \code{character} values for the climate forecasts download. Default values retrieve the current day's forecast of min, mean, and max temperature and precipitation from the Northwest Knowledge Network's North American Multi-Model Ensemble (NMME) climate forecasts.
#'
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).
#'
#' @param unzip_pause Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delay. 
#'
#' @param download_timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @return Named \code{list} of settings for the directory (for \code{directory_settings}) or \code{list} of settings components (for \code{directory_files}, \code{directory_subdirectories}, and \code{directory_resources}).
#'
#' @name directory settings
#'
NULL

#' @rdname directory-settings
#'
#' @export
#'
directory_settings <- function (files             = directory_files( ),
                                subdirectories    = directory_subdirectories( ),
                                resources         = directory_resources( ),
                                save              = TRUE,
                                overwrite         = FALSE, 
                                unzip_pause       = 30,
                                download_timeout  = getOption("timeout")) {

  list(files            = files,
       subdirectories   = subdirectories,
       resources        = resources,
       repository       = "portalPredictions", # work to remove, should be populated from internal to the resource files
       save             = save, 
       overwrite        = overwrite, 
       unzip_pause      = unzip_pause,
       download_timeout = download_timeout)

}

#' @rdname directory-settings
#'
#' @export
#'
directory_files <- function (directory_configuration = "directory_configuration.yaml",
                             moons                   = "moon_dates.csv",
                             covariates              = "covariates.csv",
                             historical_covariates   = "historical_covariates.csv",
                             forecast_covariates     = "forecast_covariates.csv",
                             dataset_controls        = "dataset_controls.yaml", 
                             model_controls          = "model_controls.yaml",
                             cast_evaluations        = "cast_evaluations.csv",
                             forecast_metadata       = "casts_metadata.csv",
                             metadata                = "metadata.yaml") {

  list(directory_configuration = directory_configuration,
       moons                   = moons,
       covariates              = covariates,
       historical_covariates   = historical_covariates,
       forecast_covariates     = forecast_covariates,
       dataset_controls        = dataset_controls, 
       model_controls          = model_controls,
       cast_evaluations        = cast_evaluations,
       forecast_metadata       = forecast_metadata,
       metadata                = metadata)

}


#' @rdname directory-settings
#'
#' @export
#'
directory_subdirectories <- function (forecasts = "forecasts", 
                                      fits      = "fits", 
                                      models    = "models", 
                                      resources = "resources", 
                                      data      = "data") {

  list(forecasts = forecasts, 
       fits      = fits, 
       models    = models, 
       resources = resources, 
       data      = data)

}

#' @rdname directory-settings
#'
#' @export
#'
directory_resources <- function (PortalData        = list(source  = "github", 
                                                          version = "latest"),
                                 portalPredictions = list(source  = "github", 
                                                          version = "latest"),
                                 climate_forecast  = list(source  = "NMME", 
                                                          version = as.character(Sys.Date()), 
                                                          data    = c("tasmin", "tasmean", "tasmax", "pr"))) {

  list(PortalData        = PortalData,
       portalPredictions = portalPredictions,
       climate_forecast  = climate_forecast)

}


#' @rdname directory-settings
#'
#' @export
#'
production_settings <- function (download_timeout  = max(getOption("timeout"), 600)) {

  resources <- directory_resources(portalPredictions = list(source  = "github", 
                                                            version = "latest"))

  directory_settings(resources        = resources,
                     download_timeout = download_timeout)

}


#' @rdname directory-settings
#'
#' @export
#'
sandbox_settings <- function ( ) {

  directory_settings()

}






