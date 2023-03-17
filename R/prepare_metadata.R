#' @title Prepare a Model-Running Metadata List
#'
#' @description Sets up the metadata used for forecasting, in particular the matching of time period across the datasets. This should always be run after \code{\link{prepare_newmoons}}, \code{\link{prepare_rodents}}, and \code{\link{prepare_covariates}} but before any model is run.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{character} vector of name(s) of dataset(s) to include.
#'
#' @param new_dataset_controls \code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets} that are to be added to the control list and file.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @return \code{list} of casting metadata, which is also saved out as a YAML file (\code{.yaml}) if desired.
#' 
#' @export
#'
prepare_metadata <- function (main                 = ".",
                              models               = prefab_models( ),
                              datasets             = prefab_datasets( ),
                              new_dataset_controls = NULL,
                              settings             = directory_settings( ), 
                              quiet                = FALSE,
                              verbose              = FALSE) {


  messageq("  - metadata file", quiet = quiet)

  config <- read_directory_configuration(main     = main, 
                                         settings = settings,
                                         quiet    = quiet)
  
  newmoons <- read_newmoons(main     = main, 
                            settings = settings)


  dataset_controls_list <- dataset_controls(main     = main, 
                                            settings = settings, 
                                            datasets = datasets)

  historic_start_newmoon <- min(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$timeseries_start >= 0)])
  historic_end_newmoon   <- max(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$origin < 0)])
  historic_newmoons      <- historic_start_newmoon:historic_end_newmoon
  forecast_start_newmoon <- min(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - settings$time$origin >= 0)])
  forecast_end_newmoon   <- max(newmoons$newmoonnumber)
  forecast_newmoons      <- forecast_start_newmoon:forecast_end_newmoon
  forecast_dates         <- as.Date(newmoons$newmoondate[match(forecast_newmoons, newmoons$newmoonnumber)])
  forecast_months        <- format(forecast_dates, "%m")
  forecast_years         <- format(forecast_dates, "%Y")
  lead_time_newmoons     <- length(forecast_newmoons)

  cast_meta  <- read_casts_metadata(main     = main, 
                                    settings = settings,
                                    quiet    = quiet)
  cast_group <- max(c(0, cast_meta$cast_group)) + 1


  out <- list(time                    = list(timeseries_start        = as.character(settings$time$timeseries_start),
                                             timeseries_start_lagged = as.character(settings$time$timeseries_start_lagged),
                                             lead_time               = settings$time$lead_time,
                                             max_lag                 = settings$time$max_lag,
                                             lag_buffer              = settings$time$lag_buffer,
                                             origin                  = as.character(settings$time$origin),
                                             historic_start_newmoon  = historic_start_newmoon,
                                             historic_end_newmoon    = historic_end_newmoon,
                                             historic_newmoons       = historic_newmoons,
                                             forecast_start_newmoon  = forecast_start_newmoon,
                                             forecast_end_newmoon    = forecast_end_newmoon,
                                             forecast_newmoons       = forecast_newmoons,
                                             forecast_years          = forecast_years,
                                             forecast_months         = forecast_months,
                                             lead_time_newmoons      = lead_time_newmoons),
              cast_group              = cast_group,
              dataset_controls        = dataset_controls_list,
              datasets                = datasets,
              models                  = models,
              confidence_level        = settings$confidence_level,
              directory_configuration = config)

  write_data(x        = out, 
             main     = main, 
             save     = settings$save, 
             filename = settings$files$metadata, 
             quiet    = !verbose)

}