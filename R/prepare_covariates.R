#' @title Prepare covariate data table
#'
#' @description Prepare covariate data for forecasting, including saving out
#'   the data and appending the new forecasts of covariates to the existing
#'   covariate forecast table
#'
#' @param moons current newmoon table
#'
#' @param options_covariates control options list for covariates
#'
#' @return covariate data table
#'
#' @export
#'
prep_covariates <- function(moons = prep_moons(),
                            options_covariates = covariates_options()){

  if (!options_covariates$quiet){
    message("Loading the covariates data files into data subdirectory")
  }
  hist_data <- prep_hist_covariates(options_covariates$tree)

  if (options_covariates$cov_fcast){
    fcast_data <- prep_fcast_covariates(hist_data, moons, options_covariates)
  }
  
  out <- hist_data[-(1:nrow(hist_data)), ]
  if (options_covariates$cov_hist){
    out <- bind_rows(out, hist_data)
  }
  if (options_covariates$cov_fcast){
    out <- bind_rows(out, fcast_data)
  }

  dataout(out, options_covariates)
}

#' @title Transfer the historical covariate data forecasts to the data folder
#'
#' @description Currently, the historical covariate data forecasts are being
#'   held in the \code{extdata} folder in the package directory, and are the
#'   transfer is hard-wired to come from there. In the future, we should 
#'   consider housing the data differently (they'll be continuously updated
#'   with the forecasts), and thus we'll want to un-hard-wire that.
#'
#' @param options_data data options
#'
#' @return nothing
#'
#' @export
#'
transfer_hist_covariate_forecasts <- function(options_data = data_options()){
  path_to <- file_path(options_data$tree, "data/covariate_forecasts.csv")
  if (!file.exists(path_to)){
    if (!options_data$quiet){
      message("Loading historical covariate forecasts into data subdirectory")
    }
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
#' @param moons current newmoon table
#'
#' @param options_covariates control options list for covariates
#'
#' @return covariate data table
#'
#' @export
#'
prep_fcast_covariates <- function(hist_data = prep_hist_covariates(),
                                  moons = prep_moons(),
                                  options_covariates = covariates_options()){

  update_covfcast_options(options_covariates, hist_data, moons) %>%
  forecast_covariates(hist_data, moons, .) %>%
  append_cov_fcast_csv(options_covariates) %>%
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

#' @title Update the data options for covariate forecasts based on existing 
#'   data
#'
#' @description Update the covariate data control options based on the 
#'   historical covariate data and moon data
#'
#' @param options_covariates covariate data options
#'
#' @param hist_data historical covariate data
#'
#' @param moons current newmoon table
#'
#' @return control options list for covariates
#'
#' @export
#'
update_covfcast_options <- function(options_covariates, hist_data, moons){

  prev_newmoon <- max(which(moons$newmoondate < options_covariates$fdate))
  prev_newmoon <- moons$newmoonnumber[prev_newmoon]
  prev_covar_newmoon <- tail(hist_data, 1)$newmoonnumber
  first_fcast_newmoon <- prev_covar_newmoon + 1
  last_fcast_newmoon <- prev_newmoon + options_covariates$lead_time
  options_covariates$fcast_nms <- first_fcast_newmoon:last_fcast_newmoon
  options_covariates$nfcnm <- length(options_covariates$fcast_nms)
  options_covariates
}


