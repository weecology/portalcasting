#' @title Build a covariate forecast for model predictions 
#' 
#' @description combining weather and ndvi forecasts
#'
#' @param covariate_data historical covariate data table
#'
#' @param moons moon data
#'
#' @param options_covariates covariate data options
#'
#' @return a data.frame with needed forecasted covariates
#'
#' @export
#'
forecast_covariates <- function(covariate_data, moons, 
                                options_covariates = covariates_options()){
  if (options_covariates$cast_type == "forecasts"){
    moons <- trim_moons_fcast(moons, options_covariates)
    weather_f <- forecast_weather(moons, options_covariates)
    ndvi_f <- forecast_ndvi(covariate_data, moons, options_covariates)
    fcast <- right_join(weather_f, ndvi_f, by = "newmoonnumber")
    forecast_newmoon <- max(moons$newmoonnumber)
    return(round(data.frame(forecast_newmoon, fcast), 3))
  }
  if (options_covariates$cast_type == "hindcasts"){
    end_step <- options_covariates$end[options_covariates$hind_step]
    path <- file_path(options_covariates$tree, "data/covariate_forecasts.csv")
    hist_fcast <- read.csv(path, stringsAsFactors = FALSE)
    nmin <- hist_fcast$newmoonnumber %in% options_covariates$fcast_nms
    esin <- hist_fcast$forecast_newmoon %in% end_step
    return(select(hist_fcast[which(nmin & esin), ], -date_made))
  }
}

#' @title Build an ndvi forecast for model predictions 
#' 
#' @description creating forecasts
#' 
#' @param covariate_data historical covariate data table
#'
#' @param moons moon data
#'
#' @param options_covariates covariate data options
#'
#' @return a data.frame with needed foecasted ndvi values
#'
#' @export
#'
forecast_ndvi <- function(covariate_data, moons, options_covariates){
  ndvi_data <- select(covariate_data, c("newmoonnumber", "ndvi"))
  ndvi_lead <- options_covariates$nfcnm - options_covariates$min_lag
  fcast_ndvi(ndvi_data, "newmoon", lead = ndvi_lead, moons)
}

#' @title Trim the moons table for covariate forecasting
#' 
#' @description creating forecasts
#'
#' @param moons moon data
#'
#' @param options_covariates covariate data options
#'
#' @return a trimmed moons data table
#'
#' @export
#'
trim_moons_fcast <- function(moons, options_covariates){
  moons <- moons[, c("newmoonnumber", "newmoondate", "period", "censusdate")]
  fc_nms <- moons
  addl_fcast <- which(moons$newmoonnumber %in% options_covariates$fcast_nms)
  if (length(addl_fcast) > 0){
    fc_nms <- fc_nms[-addl_fcast, ]
  }
  fc_nms
}

#' @title Build a weather forecast for model predictions 
#' 
#' @description creating forecasts
#' 
#' @param moons moon data
#'
#' @param options_covariates covariate data options
#'
#' @return a data.frame with needed foecasted weather values
#'
#' @export
#'
forecast_weather <- function(moons = prep_moons(), 
                             options_covariates = covariates_options()){
  
  mpath <- main_path(options_covariates$tree)
  newweather <- weather("newmoon", fill = TRUE, mpath) %>%
                select(-c(.data$locally_measured, .data$battery_low)) %>% 
                mutate(year = as.numeric(format(date, "%Y"))) %>%
                filter(year >= options_covariates$start - 5)
  incompletes <- which(is.na(newweather$newmoonnumber))
  if(length(incompletes) > 0 ){
    newweather <- newweather[-incompletes, ]  
  }
  fcasts <- get_climate_forecasts(moons, options_covariates)

  tail(newweather, options_covariates$min_lag) %>% 
  select(-year, -date) %>%
  bind_rows(fcasts)
}

#' @title Download downscaled weather data
#' 
#' @description ENSMEAN (ensemble mean) obtained from 
#'   https://climate.northwestknowledge.net/RangelandForecast/download.php
#'   and downscaled to Portal, AZ (31.9555, -109.0744)
#' 
#' @param moons newmoon data table
#'
#' @param options_covariates covariate options control list
#' 
#' @return a data.frame with precipitation(mm), temperature(C), year, and
#'   month. Temperature is the mean temperature for the month, while 
#'   precipitation is the total forecasted precip.
#'
#' @export
#'
get_climate_forecasts <- function(moons = prep_moons(), 
                                  options_covariates = covariates_options()){
  
  lead_time <- options_covariates$lead_time - options_covariates$min_lag

  if(!lead_time %in% 1:7){
    stop(paste0("Lead time must be an integer 1 - 7, got: ", lead_time))
  }

  climate_model <- "ENSMEAN"
  lat <- 31.9555
  lon <- -109.0744

  last_moon <- tail(moons, 1)
  last_moon$newmoondate <- as.Date(as.character(last_moon$newmoondate))
  future_moons <- get_future_moons(moons, num_future_moons = lead_time)
  start_time <- as.character(as.Date(last_moon$newmoondate) + 1)
  end_time <- future_moons$newmoondate[lead_time]
  days_for_forecast <- seq.Date(as.Date(start_time), as.Date(end_time), 1)
  daily_forecasts <- data.frame(date = days_for_forecast)
  start_time <- paste0(start_time, "T00%3A00%3A00Z")
  end_time <- paste0(end_time, "T00%3A00%3A00Z")
  
  base_url_1 <- "https://tds-proxy.nkn.uidaho.edu/thredds/ncss/"
  base_url_2 <- "NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/"
  base_url_3 <- "dailyForecasts/bcsd_nmme_metdata_"
  base_url <- paste(base_url_1, base_url_2, base_url_3, sep = "")
  type_urls <- c("tasmin", "tasmean", "tasmax", "pr")
  full_urls <- paste0(base_url, climate_model, "_forecast_", type_urls, 
                      "_daily.nc?var=", type_urls, "&latitude=", lat, 
                      "&longitude=", lon, "&time_start=", start_time, 
                      "&time_end=", end_time, "&accept=csv")
  raw_download <- getURL(full_urls)
  df1 <- read.table(sep = ",", skip = 1, text = raw_download[1])
  df2 <- read.table(sep = ",", skip = 1, text = raw_download[2])
  df3 <- read.table(sep = ",", skip = 1, text = raw_download[3])
  df4 <- read.table(sep = ",", skip = 1, text = raw_download[4])
  colnames(df1) <- c("date", "lat", "lon", "mintemp")
  colnames(df2) <- c("date", "lat", "lon", "meantemp")
  colnames(df3) <- c("date", "lat", "lon", "maxtemp")
  colnames(df4) <- c("date", "lat", "lon", "precipitation")
  df1$mintemp[df1$mintemp == -9999] <- NA
  df2$meantemp[df2$meantemp == -9999] <- NA
  df3$maxtemp[df3$maxtemp == -9999] <- NA
  df4$precipitation[which(df4$precipitation < 0)] <- 0
  df <- df1 %>% 
        right_join(df2, by = c("date", "lat", "lon")) %>% 
        right_join(df3, by = c("date", "lat", "lon")) %>% 
        right_join(df4, by = c("date", "lat", "lon")) %>% 
        mutate(date = as_date(date)) %>% 
        select(-lat, -lon) %>%
        mutate(mintemp = (mintemp - 32) * 5 / 9) %>%
        mutate(maxtemp = (maxtemp - 32) * 5 / 9) %>%
        mutate(meantemp = (meantemp - 32) * 5 / 9) %>%
        mutate(precipitation = precipitation * 25.4)

  daily_fcast <- full_join(daily_forecasts, df, by = "date")

  hist_path <- main_path(options_covariates$tree)
  historic <- weather("daily", fill = TRUE, path = hist_path)
  datechar <- paste(historic$year, historic$month, historic$day, sep = "-")
  historic$date <- as.Date(datechar)

  avail_historic <- which(daily_forecasts$date %in% historic$date)
  n_avail_historic <- length(avail_historic)

  if(n_avail_historic > 0){
    dates_avail <- daily_fcast$date[avail_historic]
    in_hist <- which(historic$date %in% dates_avail)
    in_fcast <- which(daily_fcast$date %in% dates_avail)
    daily_fcast$mintemp[in_fcast] <- historic$mintemp[in_hist]
    daily_fcast$meantemp[in_fcast] <- historic$meantemp[in_hist]
    daily_fcast$maxtemp[in_fcast] <- historic$maxtemp[in_hist]
    daily_fcast$precipitation[in_fcast] <- historic$precipitation[in_hist]
  }

  temp_moons <- rbind(last_moon, future_moons)
  newmoon_number <- temp_moons$newmoonnumber[-1]
  newmoon_start <- temp_moons$newmoondate[-(1 + lead_time)] 
  newmoon_end <- temp_moons$newmoondate[-1]
  newmoon_match_number <- NULL
  newmoon_match_date <- NULL
  for(i in 1:lead_time){
    date_seq <- seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1)
    if (date_seq[length(date_seq)] != newmoon_end[i]){
      date_seq <- c(date_seq, newmoon_end[i])
    }
    temp_dates <- as.character(date_seq)
    temp_numbers <- rep(newmoon_number[i], length(temp_dates))
    newmoon_match_date <- c(newmoon_match_date, temp_dates)
    newmoon_match_number <- c(newmoon_match_number, temp_numbers)   
  }  
  newmoon_match_date <- as.Date(newmoon_match_date)
  which_match <- match(daily_forecasts$date, newmoon_match_date)
  daily_fcast$newmoonnumber <- newmoon_match_number[which_match]

  out <- daily_fcast %>% 
         group_by(newmoonnumber) %>%
         summarize(mintemp = min(mintemp, na.rm = T), 
                   maxtemp = max(maxtemp, na.rm = T), 
                   meantemp = mean(meantemp, na.rm = T), 
                   precipitation = sum(precipitation, na.rm = T))

  return(out)
}

#' @title Append a covariate forecast to existing covariate forecasts
#' 
#' @description combining weather and ndvi forecasts to the existing forecasts
#' 
#' @param new_forecast_covariates forecasted covariates
#'
#' @param options_covariates control options list for covariates
#' 
#' @return new_forecast_covariates as input
#'
#' @export
#'
append_cov_fcast_csv <- function(new_forecast_covariates, 
                                 options_covariates = covariates_options()){

  if (!options_covariates$append_fcast_csv){
    return(new_forecast_covariates)
  }
  if (options_covariates$cast_type == "hindcast"){
    return(new_forecast_covariates)
  }

  covar_new <- new_forecast_covariates
  covar_new$source <- options_covariates$source_name
  covar_new$date_made <- as.character(today())

  fname <- paste0("data/", options_covariates$hist_fcast_file)
  hist_file <- file_path(options_covariates$tree, fname)

  if (file.exists(hist_file)){
    covar_hist <- read.csv(hist_file, stringsAsFactors = FALSE)
    if (covar_new$forecast_newmoon[1] > max(covar_hist$forecast_newmoon)){
      out <- rbind(covar_hist, covar_new)
    } else{
      out <- covar_hist
    }
  } else{
    out <- covar_new
  }
  write.csv(out, hist_file, row.names = FALSE)
  return(new_forecast_covariates)
}

#' @title Append a covariate forecast to historical covariate table
#' 
#' @description combining weather and ndvi forecasts to the existing 
#'   covariates
#' 
#' @param covariates output from \code{get_covariate_data}
#'
#' @param metadata model metadata
#'
#' @param moons moon data table
#' 
#' @return no value
#'
#' @export
#'
append_covariate_fcast <- function(covariates, metadata, 
                                   moons = prep_moons()){

  covariates_fcast <- forecast_covariates(covariates, moons, metadata)
  covariates_fcast <- select(covariates_fcast, -"forecast_newmoon")
  covariates_all <- bind_rows(covariates, covariates_fcast)
  return(covariates_all)
}
