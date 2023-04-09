#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param files `list` of `character` names of standard files, see [`directory_files`].
#'
#' @param subdirectories `list` of `character` names of standard subdirectories, see [`directory_subdirectories`].
#'
#' @param resources `list` of `list`s for standard resources, see [`directory_resources`].
#'
#' @param time `list` of time settings, see [`time_settings`].
#'
#' @param newmoons `character` name for the lunar data csv.
#'
#' @param covariates `character` name for the combined historical and forecast covariates csv.
#'
#' @param foreforecasts_metadata `character` name for the forecast metadata csv.
#'
#' @param metadata `character` name for the Forecast metadata YAML.
#'
#' @param datasets_controls `character` name for the YAML of datasets control list(s).
#'
#' @param models_rmd `character` name for the Rmd file for the models page of the app.
#'
#' @param models_html `character` name for the html file where the models page will be saved for the app.
#'
#' @param about_md `character` name for the md file for the about page of the app.
#'
#' @param rodents_profiles_html `character` name for the html file where the rodents profiles page will be saved for the app.
#'
#' @param rodents_profiles_csv `character` name for the csv file containing content for the rodents profiles table for the app.
#'
#' @param models_controls `character` name for the YAML of models controls list(s).
#'
#' @param foreforecasts_evaluations `character` name for the forecast evaluations csv.
#'
#' @param forecasts_results `character` name for the forecast combination results csv.
#'
#' @param resources `character` name for the resources subdirectory.
#'
#' @param data `character` name for the data subdirectory.
#'
#' @param models `character` name for the models subdirectory.
#'
#' @param fits `character` name for the fits subdirectory.
#'
#' @param forecasts `character` name for the forecasts subdirectory.
#'
#' @param app `character` name for the app subdirectory.
#'
#' @param PortalData `list` of `source` and `version` elements of `character` values for the Portal Data download. Default values retrieve the latest data from github
#'
#' @param portalPredictions `list` of `source` and `version` elements of `character` values for the archive download. Default values point to github, but `verison = NULL` indicates no download.
#'
#' @param climate_forecasts `list` of `source`, `version`, and `data` elements of `character` values for the climate forecasts download. Default values retrieve the current day's forecast of min, mean, and max temperature and precipitation from the Northwest Knowledge Network's North American Multi-Model Ensemble (NMME) climate forecasts.
#'
#' @param save `logical` indicator controlling if the output should be saved out.
#'
#' @param overwrite `logical` indicator of whether or not file writing should occur even if a local copy already exists.
#'
#' @param force `logical` indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as `FALSE`).
#'
#' @param unzip_pause Positive `integer` or integer `numeric` seconds for pausing during steps around unzipping that require time delay. 
#'
#' @param download_timeout Positive `integer` or integer `numeric` seconds for timeout on downloads. Temporarily overrides the `"timeout"` option in [`base::options`].
#'
#' @param origin `Date` forecast origin. Default is today's date (set using [`Sys.Date`]).
#'
#' @param cast_date `Date` of when the forecasts are occurring. Default is today's date (set using [`Sys.Date`]).
#'
#' @param timeseries_start `Date` after which historic samples are included in the timeseries fit. Default value is `1995-01-01`, corresponding to moon 217.
#'
#' @param lead_time `integer` (or integer `numeric`) value for the number of calendar days forward a cast will cover. \cr 
#'   As of version 0.51.0, default is now `365`, which when divided by 29.5 (duration of a lunar month), gives 13. The previous value was previously 12. We are now using 13 to align with the timestep being a lunar month, and 13 lunar months covers a full calendar year. 
#'
#' @param max_lag `integer` (or integer `numeric`) maximum number of calendar days that any covariate is lagged for prediction in a model. \cr
#'   Default is `365` for the logistic covariate models.
#'
#' @param lag_buffer `integer` (or integer `numeric`) additional number of calendar days back in time to add to the maximum lag. \cr
#'   Default value of `60` corresponds to two additional lunar months. 
#'
#' @param lead_time_buffer `integer` (or integer `numeric`) additional number of calendar days forward in time to forecast. \cr
#'   Default value of `30` corresponds to one additional lunar month. 
#'
#' @param confidence_level `numeric` confidence level used in summarizing model output. Must be between `0` and `1`.
#'
#' @param nsamples `integer` (or integer `numeric`) number of samples used to summarizing model output of sample-based estimates. 
#'
#' @return Named `list` of settings for the directory (for `directory_settings`) or `list` of settings components (for `directory_files`, `directory_subdirectories`, and `directory_resources`).
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
                                time              = time_settings( ),
                                confidence_level  = 0.95,
                                nsamples          = 1e4,
                                save              = TRUE,
                                overwrite         = TRUE, 
                                force             = FALSE, 
                                unzip_pause       = 30,
                                download_timeout  = getOption("timeout")) {

  list(files            = files,
       subdirectories   = subdirectories,
       resources        = resources,
       repository       = "portalPredictions", 
       confidence_level = confidence_level,
       nsamples         = nsamples,
       time             = time,
       save             = save, 
       force            = force, 
       overwrite        = overwrite, 
       unzip_pause      = unzip_pause,
       download_timeout = download_timeout)

}

#' @rdname directory-settings
#'
#' @export
#'
time_settings <- function (timeseries_start = as.Date("1995-01-01"), 
                           origin           = Sys.Date( ),
                           cast_date        = Sys.Date( ),
                           lead_time        = 365,
                           max_lag          = 365,
                           lag_buffer       = 60,
                           lead_time_buffer = 30) {


  timeseries_start_lagged <- timeseries_start - max_lag - lag_buffer 
  forecast_start          <- origin + 1
  forecast_end            <- origin + lead_time
  forecast_end_buffered   <- origin + lead_time + lead_time_buffer

  list(timeseries_start        = as.character(timeseries_start),
       timeseries_start_lagged = as.character(timeseries_start_lagged),
       forecast_start          = as.character(forecast_start),
       forecast_end            = as.character(forecast_end),
       forecast_end_buffered   = as.character(forecast_end_buffered),
       origin                  = as.character(origin),
       cast_date               = as.character(cast_date),
       lead_time               = lead_time,
       lead_time_buffer        = lead_time_buffer,
       max_lag                 = max_lag,
       lag_buffer              = lag_buffer)

}

#' @rdname directory-settings
#'
#' @export
#'
directory_files <- function (newmoons              = "newmoons.csv",
                             covariates            = "covariates.csv",
                             datasets_controls     = "datasets_controls.yaml", 
                             models_controls       = "models_controls.yaml",
                             foreforecasts_evaluations = "foreforecasts_evaluations.csv",
                             forecasts_results     = "forecasts_results.csv",
                             foreforecasts_metadata    = "foreforecasts_metadata.csv",
                             metadata              = "metadata.yaml",
                             about_md              = "about.md",
                             models_html           = "models.html",
                             models_rmd            = "models.Rmd",
                             rodents_profiles_html = "rodents_profiles.html",
                             rodents_profiles_csv  = "rodents_profiles.csv") {

  list(newmoons                = newmoons,
       covariates              = covariates,
       datasets_controls       = datasets_controls, 
       models_controls         = models_controls,
       foreforecasts_evaluations   = foreforecasts_evaluations,
       forecasts_results       = forecasts_results,
       foreforecasts_metadata      = foreforecasts_metadata,
       about_md                = about_md,
       models_rmd              = models_rmd,
       models_html             = models_html,
       rodents_profiles_html   = rodents_profiles_html,
       rodents_profiles_csv    = rodents_profiles_csv,
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
                                      data      = "data", 
                                      app       = "app") {

  list(forecasts = forecasts, 
       fits      = fits, 
       models    = models, 
       resources = resources, 
       data      = data, 
       app       = app)

}

#' @rdname directory-settings
#'
#' @export
#'
directory_resources <- function (PortalData         = list(source  = "github", 
                                                           version = "latest"),
                                 portalPredictions  = list(source  = "github", 
                                                           version = NULL),
                                 climate_forecasts  = list(source  = "NMME", 
                                                           version = as.character(Sys.Date()), 
                                                           data    = list(mintemp       = "tasmin",
                                                                          meantemp      = "tasmean", 
                                                                          maxtemp       = "tasmax", 
                                                                          precipitation = "pr"))) {

  list(PortalData         = PortalData,
       portalPredictions  = portalPredictions,
       climate_forecasts  = climate_forecasts)

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

  directory_settings( )

}






