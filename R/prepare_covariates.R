#' @title Prepare covariate data table
#'
#' @description Prepare covariate data for forecasting, including saving out
#'   the data and appending the new forecasts of covariates to the existing
#'   covariate forecast table
#'
#' @param tree directory tree
#'
#' @param moons current newmoon table
#'
#' @param data_options control options list for covariates
#' 
#' @param quiet logical indicator of printing messages
#'
#' @return covariate data table
#'
#' @export
#'
prep_covariates <- function(tree = dirtree(), moons = prep_moons(),
                            data_options = covariates_options(), 
                            quiet = FALSE){

  if (!quiet){
    cat("Loading the covariates data files into the data subdirectory. \n")
  }
  transfer_historical_covariate_forecasts(tree = tree)
  hist_data <- prep_hist_covariates(tree = tree)

  if (data_options$forecasts){
    fcast_data <- prep_fcast_covariates(hist_data, tree, moons, data_options)
  }
  
  out <- hist_data[-(1:nrow(hist_data)), ]
  if (data_options$historical){
    out <- bind_rows(out, hist_data)
  }
  if (data_options$forecasts){
    out <- bind_rows(out, fcast_data)
  }

  dataout(out, tree, data_options)
}

#' @title Transfer the historical covariate data forecasts to the data folder
#'
#' @description Currently, the historical covariate data forecasts are being
#'   held in the \code{extdata} folder in the package directory, and are the
#'   transfer is hard-wired to come from there. In the future, we should 
#'   consider housing the data differently (they'll be continuously updated
#'   with the forecasts), and thus we'll want to un-hard-wire that.
#'
#' @param tree directory tree
#'
#' @return nothing
#'
#' @export
#'
transfer_historical_covariate_forecasts <- function(tree = dirtree()){
  path_to <- file_path(tree, "data/covariate_forecasts.csv")
  if (!file.exists(path_to)){
    fname <- "extdata/covariate_forecasts.csv"
    path_from <- system.file(fname, package = "portalcasting")
    if (!file.exists(path_from)){
      stop("Historical covariate forecast data not found.")
    }
    temp <- read.csv(path_from, stringsAsFactors = FALSE)
    write.csv(temp, path_to, row.names = FALSE)    
  }
}

#' @title Prepare historical covariates data
#'
#' @description Create a data table of historical weather and ndvi data
#'
#' @param tree directory tree
#'
#' @return historical covariate table
#'
#' @export
#'
prep_hist_covariates <- function(tree = dirtree()){
  weather_data <- prep_weather_data(tree = tree)
  ndvi_data <- ndvi("newmoon", fill = TRUE, path = main_path(tree))
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  out$source <- "hist"
  return(out)
}

#' @title Prepare forecast covariate data table
#'
#' @description Prepare forecasts of covariate data as needed for rodent
#'   forecasting, including appending the new forecasts of covariates to the
#'   existing covariate forecast table
#'
#' @param hist_data historical covariate data
#'
#' @param tree directory tree
#'
#' @param moons current newmoon table
#'
#' @param data_options control options list for covariates
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return covariate data table
#'
#' @export
#'
prep_fcast_covariates <- function(hist_data = prep_hist_covariates(),
                                  tree = dirtree(), moons = prep_moons(),
                                  data_options = covariates_options(), 
                                  quiet = FALSE){

  update_covfcast_options(data_options, hist_data, moons) %>%
  forecast_covariates(tree, hist_data, moons, .) %>%
  append_covariate_fcast_csv(tree = tree, data_options) %>%
  select(-forecast_newmoon) %>%
  mutate("source" = "fcast")

}

#' @title Prepare historical weather data
#'
#' @description Create a data table of historical weather data
#'
#' @param tree directory tree
#'
#' @return historical weather table
#'
#' @export
#'
prep_weather_data <- function(tree = dirtree()){
  cols <- c("mintemp", "maxtemp", "meantemp", "precipitation", 
            "newmoonnumber")
  weather_data <- weather("newmoon", fill = TRUE, path = main_path(tree)) %>% 
                  ungroup() %>%
                  select(cols)
  incompletes <- which(is.na(weather_data$newmoonnumber))
  if (length(incompletes) > 0){
    weather_data <- weather_data[-incompletes, ]
  }
  return(weather_data)
}

#' @title Prepare the data options for covariates data
#'
#' @description Create a list of control options for the covariates data
#'
#' @param historical logical indicator whether or historical covariates are
#'   to be included
#'
#' @param forecasts logical indicator whether or forecasted covariates are
#'   to be included
#'
#' @param fdate date from which future is defined, typically today's date.
#'
#' @param yr year of today
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param lead_time number of moons into the future the rodents are forecast
#'
#' @param min_lag the minimum lag time used in any model
#'
#' @param fcast_nms newmoon numbers to be forecast (for the rodents, i.e. 
#'   pre-lag)
#'
#' @param nfcnm number of forecast newmoons
#'
#' @param append_fcast_csv logical if the new forecast should be appended to
#'   the historical forecasts for the purposes of hindcasting
#'
#' @param hist_fcast_file name of the file where the historical 
#'   covariate forecasts are held
#'
#' @param source_name character value for the name to give the covariaate
#'   forecast. Currently is "current_archive". Previous to "current_archive",
#'   the data were retroactively filled in and are given the source name
#'   "retroactive"
#'
#' @param save logical if the data should be saved out
#'
#' @param filename the name of the file for the saving
#'
#' @return control options list for covariates
#'
#' @export
#'
covariates_options <- function(historical = TRUE, forecasts = TRUE, 
                               fdate = today(), 
                               yr = as.numeric(format(today(), "%Y")),
                               start = 217, lead_time = 12,
                               min_lag = 6, fcast_nms = NULL, nfcnm = 0,
                               append_fcast_csv = TRUE, 
                               hist_fcast_file = "covariate_forecasts.csv",
                               source_name = "current_archive",
                               save = TRUE, filename = "covariates.csv"){
  list("historical" = historical, "forecasts" = forecasts, "fdate" = fdate,
       "yr" = yr, "start" = start, 
       "lead_time" = lead_time, "min_lag" = min_lag, 
       "fcast_nms" = fcast_nms, "nfcnm" = nfcnm, 
       "append_fcast_csv" = append_fcast_csv, 
       "hist_fcast_file" = hist_fcast_file,
       "source_name" = source_name,
       "save" = save, "filename" = filename)
}

#' @title Update the data options for covariate forecasts based on existing 
#'   data
#'
#' @description Update the covariate data control options based on the 
#'   historical covariate data and moon data
#'
#' @param data_options covariate data options
#'
#' @param hist_data historical covariate data
#'
#' @param moons current newmoon table
#'
#' @return control options list for covariates
#'
#' @export
#'
update_covfcast_options <- function(data_options, hist_data, moons){

  which_prev_newmoon <- max(which(moons$newmoondate < data_options$fdate))
  prev_newmoon <- moons$newmoonnumber[which_prev_newmoon]
  prev_covar_newmoon <- tail(hist_data, 1)$newmoonnumber
  first_fcast_covar_newmoon <- prev_covar_newmoon + 1
  last_fcast_newmoon <- prev_newmoon + data_options$lead_time
  data_options$fcast_nms <- first_fcast_covar_newmoon:last_fcast_newmoon
  data_options$nfcnm <- length(data_options$fcast_nms)
  data_options
}


