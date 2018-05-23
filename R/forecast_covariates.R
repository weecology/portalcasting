
#' @title Download Downscaled Weather Data
#' 
#' @description ENSMEAN (ensemble mean) obtained from 
#'   https://climate.northwestknowledge.net/RangelandForecast/download.php
#'   and downscaled to Portal, AZ (31.9555, -109.0744)
#' 
#' @param climate_model Individual climate models available are 
#'   c("CFSv2","CMC1","CMC2","GFDL-FLOR","GFDL","NASA","NCAR"),
#'   "ENSMEAN" is the mean of all models.
#' @param lead_time the newmoons into the future to obtain forecasts. Max of 7
#' @param moons newmoon data table
#' 
#' @return a data.frame with precipitation(mm), temperature(C), year, and
#'   month. Temperature is the mean temperature for the month, while 
#'   precipitation is the total forecasted precip.
#'
#' @export
#'
get_climate_forecasts <- function(lead_time = 6, moons){
  
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
  df4$precipitation[which(df4$precipitation < 0)] <- 0
  df <- df1 %>% 
        right_join(df2) %>% 
        right_join(df3) %>% 
        right_join(df4) %>% 
        mutate(date = as_date(date)) %>% 
        select(-lat, -lon) %>%
        mutate(mintemp = (mintemp - 32) * 5 / 9) %>%
        mutate(maxtemp = (maxtemp - 32) * 5 / 9) %>%
        mutate(meantemp = (meantemp - 32) * 5 / 9) %>%
        mutate(precipitation = precipitation * 25.4)

  daily_fcast <- full_join(daily_forecasts, df)

  historic <- weather("daily", fill = TRUE)
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

#' @title Build a Weather Forecast for Model Predictions 
#' 
#' @description created using downscaled climate forecasts
#' 
#' @param start end year of data and beginning of forecast
#' @param moons moon data
#' @param lag newmoons by which weather data is lagged
#' @param lead_time the number of newmoons into the future to obtain forecasts
#'   Max of 7. lag + lead_time should equal 12
#' 
#' @return a data.frame with 12 new moons of weather values
#'
#' @export
#'
fcast_weather <- function(start = as.numeric(format(Sys.Date(), "%Y")), moons,
                          lag = 6, lead_time = 6){
  
  newweather <- weather("newmoon", fill = TRUE) %>%
                select(-c(locally_measured, battery_low)) %>% 
                mutate(year = as.numeric(format(date, "%Y"))) %>%
                filter(year >= start - 5)
  incompletes <- which(is.na(newweather$newmoonnumber))
  if(length(incompletes) > 0 ){
    newweather <- newweather[-incompletes, ]  
  }
  fcasts <- get_climate_forecasts(lead_time = lead_time, moons = moons)
  weatherforecast <- tail(newweather, lag) %>% 
                     select(-year, -date) %>%
                     bind_rows(fcasts)

  return(weatherforecast)
}


#' @title Build a Covariate Forecast for Model Predictions 
#' 
#' @description combining weather and ndvi forecasts
#' 
#' @param model_metadata metadata list for the forecast models
#' @param moons moon data
#' @param covariate_data historical covariate data table
#' 
#' @return a data.frame with 12 new moons of weather values
#'
#' @export
#'
fcast_covariates <- function(model_metadata, moons, covariate_data){

  forecast_date <- model_metadata$forecast_date
  yr <- as.numeric(format(as.Date(forecast_date), "%Y"))
  covariate_fcast_nms <- model_metadata$covariate_forecast_newmoons
  ncfnm <- length(covariate_fcast_nms)

  ndvi_data <- select(covariate_data, c("newmoonnumber", "ndvi"))

  moons <- moons[, c("newmoonnumber", "newmoondate", "period", "censusdate")]
  fc_nms <- moons
  addl_need_to_fcast <- which(moons$newmoonnumber %in% covariate_fcast_nms)
  if (length(addl_need_to_fcast) > 0){
    fc_nms <- fc_nms[-addl_need_to_fcast, ]
  }

  weather_fcast <- fcast_weather(yr, fc_nms, lag = 0, lead_time = ncfnm - 6)
  ndvi_fcast <- fcast_ndvi(ndvi_data, "newmoon", lead =  ncfnm - 6, moons)

  fcast <- right_join(weather_fcast, ndvi_fcast, by = "newmoonnumber")
  out <- data.frame(forecast_date, fcast)
  return(out)
}