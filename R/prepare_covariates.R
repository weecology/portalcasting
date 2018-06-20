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
#' @return covariate data table
#'
#' @export
#'
prep_fcast_covariates <- function(hist_data = prep_hist_covariates(),
                                  tree = dirtree(), moons = prep_moons(),
                                  data_options = covariates_options()){

  data_options <- update_covfcast_options(data_options, hist_data, moons)
  fcast_data <- forecast_covariates(tree, hist_data, moons, data_options)

# working here


}

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
#' @return covariate data table
#'
#' @export
#'
prep_covariates <- function(tree = dirtree(), moons = prep_moons(),
                            data_options = covariates_options()){

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
#' @param lead_time number of moons into the future the rodents are forecast
#'
#' @param min_lag the minimum lag time used in any model
#'
#' @param fcast_nms newmoon numbers to be forecast
#'
#' @param nfcnm number of forecast newmoons
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
                               lead_time = 12,
                               min_lag = 6, fcast_nms = NULL, nfcnm = 0,
                               save = TRUE, filename = "covariates.csv"){
  list("historical" = historical, "forecasts" = forecasts, "fdate" = fdate,
       "yr" = yr, 
       "lead_time" = lead_time, "min_lag" = min_lag, 
       "fcast_nms" = fcast_nms, "nfcnm" = nfcnm, 
       "save" = save, "filename" = filename)
}

#' @title Update the data options for covariate forecasts based on existing 
#'   data
#'
#' @description Update the covariate data control options based on the 
#'   historical covariate data and moon data
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


