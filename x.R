#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
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
#' @param models \code{vector} of models to be written via 
#'        \code{\link{write_model}}.
#'
#' @param datasets \code{list} of datasets to be created using 
#'        \code{\link{do.call}} on the defined functions. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{datasets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
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
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{datasets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
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
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.


(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_model = NULL,

                          control_files = files_control(),
                      datasets = prefab_rodent_datasets(),
                          quiet = FALSE, verbose = TRUE){






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



end_moon = NULL, 
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

#'
_dir <- function (main  = ".",
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      models = prefab_models(),
                      datasets = prefab_rodent_datasets(),
                      quiet = FALSE,
                      verbose = TRUE,
                        controls_model = NULL, 
                        control_files = files_control(), 
                        end_moon = NULL, start_moon = 217, lead_time = 12,
 min_lag = 6, 
                        confidence_level = 0.95, cast_date = Sys.Date())
                        
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


#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param filename \code{character} value of the path to the directory config YAML.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param downloads_versions \code{character} vector returned from \code{\link{fill_raw}} of the successfully downloaded versions of downloaded data.


files_control <- function(directory = "portalPredictions",
                          raw_data = "PortalData",
                          filename_moons = "moon_dates.csv",
                          filename_config = "dir_config.yaml", 
                          filename_cov = "covariates.csv", 
                          filename_cov_casts = "covariate_casts.csv",
                          filename_meta = "metadata.yaml", 
                          save = TRUE, overwrite = TRUE, cleanup = TRUE,
                          source_name = "current_archive",
                          append_cast_csv = TRUE){
  
  list(directory = directory, raw_data = raw_data, 
       filename_moons = filename_moons, filename_cov = filename_cov,
       filename_cov_casts = filename_cov_casts,
       filename_config = filename_config, filename_meta = filename_meta,
       save = save, overwrite = overwrite, cleanup = cleanup,
       source_name = source_name, append_cast_csv = append_cast_csv)

}


#'
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'                  data directory. A standard portalcasting directory
#'                  downloads the raw data files into from the PortalData 
#'                  repository, so \code{raw_data = "PortalData"}.
#'
#' @param moons \code{character} name of the file for saving the 
#'                        moons data.
#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param cleanup \code{logical} indicator of whether or not the tmp files
#'  should be cleaned up.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts later use.
#'
#' @param source \code{character} value for the name to give the covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
