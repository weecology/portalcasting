#' @title Create and fill a forecasting directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}}
#'  to create a ready-to-run (via \code{\link{portalcast}}) directory where
#'  indicated. \cr \cr
#'  \code{setup_production} creates a standard production directory. \cr \cr
#'  \code{setup_sandbox} creates a sandboxing directory. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_model Additional controls for models not in the prefab
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param datasets \code{list} of datasets to be created using 
#'        \code{\link{do.call}} on the defined functions. 
#'
#' @param PortalData_source,PortalData_version \code{character} values for the
#'        source and version of the Portal Data to download. 
#'        \cr \cr 
#'        Default values retrieve the latest data from github.
#'        \cr \cr 
#'        See \code{\link[portalr]{download_observations}}.
#'
#' @param portalPredictions_source,portalPredictions_version \code{character} 
#'        values for the source and version of the archive to download.
#'        \cr \cr 
#'        Default values point to github, but \code{version = NULL} indicates
#'        no download.
#'        \cr \cr 
#'        See \code{\link{download_archive}}.
#'
#'
#' @param climate_forecast_source,climate_forecast_version  \code{character} 
#'        values for the source and version of the climate forecasts to
#'        download.
#'        \cr \cr 
#'        Default values retrieve the current day's forecast from the
#'        Northwest Knowledge Network's North American Multi-Model Ensemble 
#'        (NMME) climate forecasts.
#'        \cr \cr 
#'        See \code{\link{download_climate_forecasts}}.
#'
#' @param bline \code{logical} indicator if a horizontal break lines should be
#'  included in messages.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages).
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
setup_dir <- function(main = ".", models = prefab_models(), end_moon = NULL, 
                      start_moon = 217, lead_time = 12, 
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

                      bline = TRUE, quiet = FALSE, verbose = FALSE){

  version_message(quiet = !bline | quiet)
  create_dir(main = main, filename_config = control_files$filename_config,
             quiet = quiet)



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

