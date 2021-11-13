#' @title Create and Fill a Forecasting Directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}} to create a ready-to-run (via \code{\link{portalcast}}) directory where indicated. \cr \cr
#'              \code{setup_production} creates a standard production directory. \cr \cr
#'              \code{setup_sandbox} creates a sandboxing directory. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{list} of datasets to be created using \code{\link{do.call}} on the defined functions. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @return \code{NULL}
#'
#' @examples
#' \donttest{
#'  setup_dir()
#'  setup_sandbox()
#'  setup_production()
#' }
#'
#' @export
#'
setup_dir <- function (main     = ".",
                       settings = directory_settings(), 
                       models   = prefab_models(), 
                       datasets = prefab_rodent_datasets(),
                       quiet    = FALSE, 
                       verbose  = FALSE){

  version_message(quiet = quiet)

  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet)



  fill_dir(main = main,

                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
               models = models, 
                      datasets = datasets,
           controls_model = controls_model, 
           control_files = control_files, 

           end_moon = end_moon, lead_time = lead_time, 
           cast_date = cast_date, start_moon = start_moon, 
           confidence_level = confidence_level, 
           quiet = quiet, verbose = verbose)
  setup_completion_message(quiet = quiet)
}



#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(main = ".", models = prefab_models(), 
                             end_moon = NULL, start_moon = 217, 
                             lead_time = 12, confidence_level = 0.95, 
                             cast_date = Sys.Date(), controls_model = NULL,

                             control_files = files_control(), 
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = "latest",
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                             quiet = FALSE, verbose = TRUE, 
                      datasets = prefab_rodent_datasets()){
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
            quiet = quiet, verbose = verbose, 
                      datasets = datasets,
                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
            control_files = control_files)
}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_model = NULL,

                          control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      datasets = prefab_rodent_datasets(),
                          quiet = FALSE, verbose = TRUE){
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
                             PortalData_version = PortalData_version,
                      datasets = datasets,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
quiet = quiet, verbose = verbose, 
            control_files = control_files)
  sandbox_welcome(main = main, quiet = quiet)
}

