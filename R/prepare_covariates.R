#' @title Collect Historic Weather Data
#'
#' @description Gather the downloaded historical weather data, select the 
#'   columns needed, and remove the incomplete data
#'
#' @return weather data table 
#'
#' @export
#'
prep_weather_data <- function(){
  cols <- c("mintemp", "maxtemp", "meantemp", "precipitation", 
            "newmoonnumber")
  weather_data <- weather("newmoon", fill = TRUE) %>% 
                  ungroup() %>%
                  select(cols)
  incompletes <- which(is.na(weather_data$newmoonnumber))
  if (length(incompletes) > 0){
    weather_data <- weather_data[-incompletes, ]
  }
  return(weather_data)
}

#' @title Collect Historic Weather and NDVI Data
#'
#' @description Gather the downloaded historical weather and ndvi data and
#'   join them into a single data table
#'
#' @return covariate data table 
#'
#' @export
#'
prep_hist_covariate_data <- function(){
  weather_data <- prep_weather_data()
  ndvi_data <- ndvi("newmoon", fill = TRUE)
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  return(out)
}

#' @title Prepare covariate data table
#'
#' @description Create a covariate (weather and NDVI) data table, including
#'   potentially both historical and forecast data
#'
#' @param moons current newmoonnumber table
#'
#' @param forecast_date date from which forecasting is happening, typically
#'   today's date. 
#'
#' @param historical logical input dictating if historical covariates should
#'   be included in the output
#'
#' @param forecasts logical input dictating if covariate forecasts should be
#'   made and included in the output
#'
#' @param lead_time number of newmoons forward to forecast abundances
#'
#' @param min_lag minimum (of non-0) lag times that need to be accommodate 
#'
#' @param append_fcast_csv logical input dictating if covariate forecasts
#'   should be added to the existing covariate forecast data file
#'
#' @param covariate_file file name for the existing historical covariate data
#'   file
#'
#' @param source_name character value for the name to give the covariaate
#'   forecast. Currently is "current_archive". Previous to "current_archive",
#'   the data were retroactively filled in and are given the source name
#'   "retroactive"
#'
#' @param data_dir directory name where the data files will reside
#'
#' @return covariate data table including both historical and covariate data
#'   with an additional column (\code{source}) distinguishing the source of
#'   the data. In addition, if \code{append_fcast_csv} is \code{TRUE}, the
#'   forecasts will be appended to \code{covariate_file} tagged with  
#'   \code{source_name} dictating the source name tag in the forecast file.
#'
#' @export
#'
prep_covariate_data <- function(moons = prep_moon_data(future = TRUE), 
                                forecast_date = Sys.Date(),
                                historical = TRUE, forecasts = TRUE,
                                lead_time = 12, min_lag = 6,
                                append_fcast_csv = FALSE, 
                                covariate_file = NULL,
                                source_name = "current_archive",
                                data_dir = pc_path("data", "~")){

  hist_data <- prep_hist_covariate_data()
  hist_data$source <- "hist"

  if (forecasts){
    which_prev_newmoon <- max(which(moons$newmoondate < forecast_date))
    prev_newmoon <- moons$newmoonnumber[which_prev_newmoon]
    prev_covar_newmoon <- tail(hist_data, 1)$newmoonnumber
    first_fcast_covar_newmoon <- prev_covar_newmoon + 1
    last_fcast_newmoon <- prev_newmoon + lead_time
    fcast_nms <- first_fcast_covar_newmoon:last_fcast_newmoon
    fcast_data <- fcast_covariates(hist_data, moons, fcast_nms, forecast_date,
                                   min_lag)

    if (append_fcast_csv){
      append_covariate_fcast_csv(fcast_data, covariate_file, source_name)
    }
    fcast_data <- select(fcast_data, -forecast_newmoon)
    fcast_data$source <- "fcast"
  }
  
  out <- hist_data[-(1:nrow(hist_data)), ]
  if (historical){
    out <- bind_rows(out, hist_data)
  }
  if (forecasts){
    out <- bind_rows(out, fcast_data)
  }
  write.csv(out, full_path("covariates.csv", data_dir), row.names = FALSE)
  return(out)
}

