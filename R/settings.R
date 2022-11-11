#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param files \code{list} of \code{character} names for standard files: 
#'   \describe{
#'     \item{directory_configuration}{Directory configuration YAML}
#'     \item{moons}{Lunar data csv}
#'     \item{covariates}{Combined historical and forecast covariates csv}
#'     \item{historical_covariates}{Historical covariates csv}
#'     \item{forecast_covariates}{Forecast covariates csv}
#'     \item{metadata}{Forecast metadata YAML}
#'     \item{dataset_controls}{YAML of dataset control list(s)}
#'     \item{model_controls}{YAML of model controls list(s)}
#'     \item{cast_evaluations}{Forecast evaluations csv}
#' }
#'
#' @param subdirectories \code{list} of subdirectories: \code{resources}, \code{data}, \code{models}, \code{fits}, and \code{forecasts}. 
#'
#' @param resources \code{list} of \code{list}s for standard resources: 
#'   \describe{
#'     \item{PortalData}{\code{source} and \code{version} elements of \code{character} values for the Portal Data download. Default values retrieve the latest data from github}
#'     \item{portalPredictions}{\code{source} and \code{version} elements of \code{character} values for the archive download. Default values point to github, but \code{version = NULL} indicates no download}
#'     \item{climate_forecast}{\code{source}, \code{version}, and \code{data} elements of \code{character} values for the climate forecasts download. Default values retrieve the current day's forecast of min, mean, and max temperature and precipitation from the Northwest Knowledge Network's North American Multi-Model Ensemble (NMME) climate forecasts}
#'   }
#'
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing files should be updated (most users should leave as \code{TRUE}).
#'
#' @param unzip_pause Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delay. 
#'
#' @param download_timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @return Named \code{list} of settings for the directory.
#'
#' @name directory settings
#'
NULL

#' @rdname directory-settings
#'
#' @export
#'
directory_settings <- function (files             = list(directory_configuration = "directory_configuration.yaml",
                                                         moons                   = "moon_dates.csv",
                                                         covariates              = "covariates.csv",
                                                         historical_covariates   = "historical_covariates.csv",
                                                         forecast_covariates     = "forecast_covariates.csv",
                                                         dataset_controls        = "dataset_controls.yaml", 
                                                         model_controls          = "model_controls.yaml",
                                                         cast_evaluations        = "cast_evaluations.csv",
                                                         metadata                = "metadata.yaml"),
                                subdirectories    = list(forecasts = "forecasts", 
                                                         fits      = "fits", 
                                                         models    = "models", 
                                                         resources = "resources", 
                                                         data      = "data"),
                                resources         = list(PortalData        = list(source  = "github", 
                                                                                  version = "latest"),
                                                         portalPredictions = list(source  = "github", 
                                                                                  version = NULL),
                                                         climate_forecast  = list(source  = "NMME", 
                                                                                  version = as.character(Sys.Date()), 
                                                                                  data    = c("tasmin", "tasmean", "tasmax", "pr"))),
                                save              = TRUE,
                                overwrite         = TRUE, 
                                unzip_pause       = 30,
                                download_timeout  = getOption("timeout")) {

  list(files            = files,
       subdirectories   = subdirectories,
       resources        = resources,
       repository       = "portalPredictions",
       save             = save, 
       overwrite        = overwrite, 
       unzip_pause      = unzip_pause,
       download_timeout = download_timeout)

}


#' @rdname directory-settings
#'
#' @export
#'
production_settings <- function ( ) {

  resources        <- list(PortalData        = list(source  = "github", 
                                                    version = "latest"),
                           portalPredictions = list(source  = "github", 
                                                    version = "latest"),
                           climate_forecast  = list(source  = "NMME", 
                                                    version = as.character(Sys.Date()), 
                                                    data    = c("tasmin", "tasmean", "tasmax", "pr")))

  download_timeout <- min(getOption("timeout"), 600)


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






