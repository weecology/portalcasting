#' @title Prepare a Model-Running Metadata List
#'
#' @description Sets up the metadata used for forecasting, in particular the matching of time period across the datasets, according to the [`directory_settings`].
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param datasets `character` vector of name(s) of dataset(s) to include.
#'
#' @param new_datasets_controls `list` of controls for any new datasets (not in the prefab datasets) listed in `datasets` that are to be added to the control list and file.
#'
#' @return `list` of casting metadata, which is also saved out as a YAML file (`.yaml`) if desired.
#' 
#' @export
#'
prepare_metadata <- function (main                 = ".",
                              datasets             = prefab_datasets( ),
                              new_datasets_controls = NULL) {

  settings <- read_directory_settings(main = main)

  messageq("  - metadata", quiet = settings$quiet)

  config <- read_directory_configuration(main = main)
  
  newmoons <- read_newmoons(main = main)
  datasets_controls_list <- datasets_controls(main     = main, 
                                              datasets = datasets)

  historic_start_newmoonnumber <- min(newmoons$newmoonnumber[newmoons$newmoondate >= settings$time$timeseries_start])
  historic_end_newmoonnumber   <- max(newmoons$newmoonnumber[newmoons$newmoondate < settings$time$origin])
  historic_newmoonnumbers      <- historic_start_newmoonnumber:historic_end_newmoonnumber
  forecast_start_newmoonnumber <- min(newmoons$newmoonnumber[newmoons$newmoondate >= settings$time$origin])
  forecast_end_newmoonnumber   <- max(newmoons$newmoonnumber[newmoons$newmoondate < settings$time$forecast_end_buffered])
  forecast_newmoonnumbers      <- forecast_start_newmoonnumber:forecast_end_newmoonnumber
  forecast_dates               <- as.Date(newmoons$newmoondate[match(forecast_newmoonnumbers, newmoons$newmoonnumber)])
  forecast_months              <- format(forecast_dates, "%m")
  forecast_years               <- format(forecast_dates, "%Y")
  lead_time_newmoons           <- length(forecast_newmoonnumbers)

  cast_meta  <- read_forecasts_metadata(main = main)
  forecast_group <- max(c(0, cast_meta$forecast_group), na.rm = TRUE) + 1


  out <- list(time                    = list(timeseries_start             = as.character(settings$time$timeseries_start),
                                             timeseries_start_lagged      = as.character(settings$time$timeseries_start_lagged),
                                             forecast_end                 = as.character(settings$time$forecast_end),
                                             forecast_end_buffered        = as.character(settings$time$forecast_end_buffered),
                                             lead_time                    = settings$time$lead_time,
                                             max_lag                      = settings$time$max_lag,
                                             lag_buffer                   = settings$time$lag_buffer,
                                             cast_date                    = as.character(settings$time$cast_date),
                                             origin                       = as.character(settings$time$origin),
                                             historic_start_newmoonnumber = historic_start_newmoonnumber,
                                             historic_end_newmoonnumber   = historic_end_newmoonnumber,
                                             historic_newmoonnumbers      = historic_newmoonnumbers,
                                             forecast_start_newmoonnumber = forecast_start_newmoonnumber,
                                             forecast_end_newmoonnumber   = forecast_end_newmoonnumber,
                                             forecast_newmoonnumbers      = forecast_newmoonnumbers,
                                             forecast_years               = forecast_years,
                                             forecast_months              = forecast_months,
                                             lead_time_newmoons           = lead_time_newmoons),
              forecast_group              = forecast_group,
              datasets_controls       = datasets_controls_list,
              confidence_level        = settings$confidence_level,
              nsamples                = settings$nsamples,
              directory_configuration = config)

  write_data(x         = out, 
             main      = main, 
             save      = settings$save, 
             filename  = settings$files$metadata, 
             overwrite = settings$overwrite, 
             quiet     = !settings$verbose)

}