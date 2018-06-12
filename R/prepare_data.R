#' @title Prepare historic rodent data
#'
#' @description Prepare the rodent dataset, tailored for forecasting. 
#'   Specifically, return data for all plots and control plots only, for each 
#'   species except PI and the total
#'
#' @param moons current newmoonnumber table
#'
#' @param start_newmoonnumber newmoon number of the first sample to be 
#'   included. Default value is \code{217}, corresponding to 1995-01-01.
#' 
#' @param data_dir directory name where the data files will reside
#'
#' @return a list of two dataframes, all plots and control plots
#'
#' @export
#'
prep_rodent_data <- function(moons = prep_moon_data(), 
                             start_newmoonnumber = 217,
                             data_dir = pc_path("data", "~")){

  controls <- abundance(clean = FALSE, level = "Treatment", type = "Rodents", 
                        length = "Longterm", min_plots = 24) %>%
              select(-PI)
  controls <- controls %>%
              mutate(total = rowSums(controls[ , -(1:2)])) %>%
              filter(treatment == "control") %>%
              select(-treatment) %>%
              inner_join(moons, by = c("period" = "period")) %>%
              subset(newmoonnumber >= start_newmoonnumber) %>%
              select(-newmoondate, -censusdate)

  all <- abundance(clean = FALSE, level = "Site", type = "Rodents", 
                        length = "all", min_plots = 24) %>%
         select(-PI)
  all <- all %>%
         mutate(total = rowSums(all[ , -1])) %>%
         inner_join(moons, by = c("period" = "period")) %>%
         subset(newmoonnumber >= start_newmoonnumber) %>%
         select(-newmoondate, -censusdate)

  out <- list("controls" = controls, "all" = all)

  write.csv(all, full_path("all.csv", data_dir), row.names = FALSE)
  write.csv(controls, full_path("controls.csv", data_dir), row.names = FALSE)
  return(out)
}

#' @title Prepare temporal (lunar) data
#'
#' @description Get time information (calendar dates, census periods, 
#'   newmoon numbers) associated with trapping events (achieved and missed)
#'   based on a lunar survey schedule.
#'
#' @param future logical value indicating if future moons (for forecasts) are
#'   to be added to the table or not
#'
#' @param forecast_date date from which forecasting is happening, typically
#'   today's date. Required if \code{future = TRUE}
#'
#' @param data_dir directory name where the data files will reside
#'
#' @return data table of time variables for all surveys
#'
#' @export
#' 
prep_moon_data <- function(future = FALSE, forecast_date = Sys.Date(),
                           data_dir = pc_path("data", "~")){

  path <- full_path("PortalData/Rodents/moon_dates.csv", "~")
  moons <- read.csv(path, header = TRUE)
  
  moons$year <- year(moons$newmoondate)
  moons$month <- month(moons$newmoondate)
  moons$newmoondate <- as.Date(as.character(moons$newmoondate))

  if (future == TRUE){
    if (!is.Date(forecast_date)){
      stop("forecast_date must be a date.")
    }
    curr_moons <- moons %>%
                  select(newmoonnumber, newmoondate, period, censusdate)
    future_moons <- get_future_moons(moons)
    total_moons <- rbind(curr_moons, future_moons)

    not_future_moons <- which(future_moons$newmoondate < forecast_date)
    nnfm <- length(not_future_moons)
    if (nnfm > 0){
      add_nfm <- future_moons[not_future_moons, ]
      add_nfm$year <- as.numeric(format(add_nfm$newmoondate, "%Y"))
      add_nfm$month <- as.numeric(format(add_nfm$newmoondate, "%m"))
      moons <- rbind(moons, add_nfm)
      curr_moons <- moons %>% 
                    select(newmoonnumber, newmoondate, period, censusdate)
      future_moons <- get_future_moons(moons)
      total_moons <- rbind(curr_moons, future_moons)
    }
    moons <- total_moons
  }
  write.csv(moons, full_path("moons.csv", data_dir), row.names = FALSE)
  return(moons)
}

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

