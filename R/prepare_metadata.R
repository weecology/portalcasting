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
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
#'
#' @param origin \code{Date} from which future is defined. In the recurring forecasting, is set to today's date using \code{\link{Sys.Date}}.
#'
#' @param timeseries_start \code{Date} after which samples are included. Default value is \code{1995-01-01}, corresponding to moon 217.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the number of days forward a cast will cover. \cr 
#'   As of version 0.51.0, default is now 365, which when divided by 29.5 (duration of a lunar month), gives 13. The previous value was previously 12. We are now using 13 to align with the timestep being a lunar month, and 13 lunar months covers a full calendar year. 
#'
#' @param confidence_level \code{numeric} confidence level used in summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @return \code{list} of casting metadata, which is also saved out as a YAML file (\code{.yaml}) if desired.
#' 
#' @export
#'
prepare_metadata <- function (main             = ".",
                              models           = prefab_models(), 
                              datasets         = prefab_datasets(),
                              timeseries_start = as.Date("1995-01-01"), 
                              origin           = Sys.Date(), 
                              lead_time        = 365,
                              max_lag          = 365,
                              confidence_level = 0.95,
                              settings         = directory_settings(), 
                              quiet            = FALSE, 
                              verbose          = FALSE) {


  messageq("  - metadata file", quiet = quiet)

  config <- read_directory_configuration(main     = main, 
                                         settings = settings,
                                         quiet    = quiet)
  
  newmoons <- read_newmoons(main     = main, 
                            settings = settings)


  historic_start_newmoon <- min(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - timeseries_start >= 0)])
  historic_end_newmoon   <- max(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - origin < 0)])
  forecast_start_newmoon <- min(newmoons$newmoonnumber[which(as.Date(newmoons$newmoondate) - origin >= 0)])
  forecast_end_newmoon   <- max(newmoons$newmoonnumber)


  out <- list(time                    = list(timeseries_start       = as.character(timeseries_start),
                                             lead_time              = lead_time,
                                             origin                 = as.character(origin),
                                             historic_start_newmoon = historic_start_newmoon,
                                             historic_end_newmoon   = historic_end_newmoon,
                                             forecast_start_newmoon = forecast_start_newmoon,
                                             forecast_end_newmoon   = forecast_end_newmoon),
              confidence_level        = confidence_level,
              directory_configuration = config)

  write_data(x        = out, 
             main     = main, 
             save     = settings$save, 
             filename = settings$files$metadata, 
             quiet    = !verbose)

}