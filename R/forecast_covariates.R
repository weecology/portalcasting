
prep_fcast_covariates <- function(hist_cov = NULL, cast_type = "forecast",
                                  moons = prep_moons(main = main),
                                  cast_date = Sys.Date(), end_moon = NULL, 
                                  lead_time = 12, min_lag = 6, 
                                  raw_path_archive = "portalPredictions",
                                  raw_covfcast_file = 
                                    "data/covariate_forecasts.csv",
                                  raw_path_covfcast = "cov_fcasts", 
                                  source_name = "current_archive",
                                  controls = list(),
                                  main = ".", quiet = FALSE, save = TRUE, 
                                  filename = "covariates.csv",
                                  overwrite = TRUE){

  fcast <- pass_and_call(forecast_covariates)
  x <- pass_and_call(save_cov_fcast_csv, new_forecast_covariates = fcast)
  fcast %>%
  select(-forecast_newmoon) %>%
  mutate("source" = "fcast")
}


save_cov_fcast_csv <- function(new_forecast_covariates = NULL, 
                                 cast_type = "forecast",
                                 append_fcast_csv = TRUE,
                                  raw_path_archive = "portalPredictions",
                                  raw_covfcast_file = 
                                    "data/covariate_forecasts.csv",
                                source_name = "current_archive",
                               save = TRUE, filename = "covariates.csv",
                               overwrite = TRUE, quiet = FALSE){

  return_if_null(new_forecast_covariates)
  if(!save | !append_fcast_csv | !overwrite | cast_type == "hindcast"){
    return(new_forecast_covariates)
  }
  new_fcast <- new_forecast_covariates
  new_fcast$source <- source_name

  date_made <- Sys.time()
  tz <- format(date_made, "%Z")
  new_fcast$date_made <- paste0(date_made, " ", tz)


  hist_file <- file_paths(main, raw_covfcast_file)
  if(file.exists(hist_file)){
    msg <- "covariates_forcasts.csv exists and overwrite = TRUE; file saved"
    hist_fcast <- read.csv(hist_file, stringsAsFactors = FALSE)
    out <- rbind(hist_fcast, new_fcast)
  } else{
    msg <- "covariates_forcasts.csv does not exist already; file saved"
    lpath <- paste0("raw/", raw_path_archive, "/", raw_covfcast_file)
    dl_hist_file <- file_paths(main, lpath)
    if(file.exists(dl_hist_file)){
      hist_fcast <- read.csv(dl_hist_file, stringsAsFactors = FALSE)
      out <- rbind(hist_fcast, new_fcast)
    } else {
      out <- new_fcast
    }      
  }
  messageq(msg, quiet)
  write.csv(out, hist_file, row.names = FALSE)
  new_forecast_covariates
}

forecast_covariates <- function(hist_cov = NULL, cast_type = "forecast",
                                  moons = prep_moons(main = main),
                                  cast_date = Sys.Date(), end_moon = NULL, 
                                  lead_time = 12, min_lag = 6, 
                                  raw_path_archive = "portalPredictions",
                                  raw_covfcast_file = 
                                    "data/covariate_forecasts.csv",
                                  raw_path_covfcast = "cov_fcasts", 
                                  controls = list(),
                                  main = ".", quiet = FALSE){

  fcast_nms <- pass_and_call(target_newmoons)
  if(cast_type == "forecast"){
    weather_f <- pass_and_call(forecast_weather, fcast_nms = fcast_nms)
    ndvi_f <- forecast_ndvi(hist_cov, moons, lead_time, min_lag)
    fcast <- right_join(weather_f, ndvi_f, by = "newmoonnumber")
    which_forecast_newmoon <- max(which(moons$newmoondate < cast_date))
    forecast_newmoon <- moons$newmoonnumber[which_forecast_newmoon]
    out <- round(data.frame(forecast_newmoon, fcast), 3)
  } else if (cast_type == "hindcast"){
    lpath <- paste0("raw/", raw_path_archive, "/", raw_covfcast_file)
    pth <- file_paths(main, lpath)
    hist_fcast <- read.csv(pth, stringsAsFactors = FALSE)
    target_in <- hist_fcast$newmoonnumber %in% fcast_nms
    origin_in <- hist_fcast$forecast_newmoon %in% end_moon 
    out <- select(hist_fcast[which(target_in & origin_in), ], -date_made)
  } else {
    stop("cast_type can only be forecast or hindcast")
  }
  out
}




forecast_weather <- function(moons = prep_moons(main = main), 
                             cast_date = Sys.Date(),
                             lead_time = 12, min_lag = 6, fcast_nms = NULL,
                             main = ".",  
                             raw_path_covfcast = "cov_fcasts", quiet = FALSE, 
                             controls = list()){
  moons0 <- trim_moons_fcast(moons, fcast_nms)
  raw_path <- sub_paths(main, "raw")
  window <- pass_and_call(forecast_window)
  controls <- update_list(controls, start = window$start, end = window$end)

# these can/should get combined?
  urls <- pass_and_call(download_climate_forecasts, controls = controls)

  fcast_weather <- pass_and_call(read_climate_forecasts, controls = controls,
                                 urls = urls)



  weather("daily", TRUE, raw_path) %>% 
  add_date_from_components() %>%
  select(-c(year, month, day, battery_low, locally_measured))  %>%
  combine_hist_and_fcast(fcast_weather) %>% 
  add_newmoons_from_date(moons) %>%
  summarize_daily_fcast_weather_by_newmoon()
}


combine_hist_and_fcast <- function(hist_tab, fcast_tab){
  dupes <- which(fcast_tab$date %in% hist_tab$date)
  if(length(dupes) > 0){
    fcast_tab <- fcast_tab[-dupes, ]
  }
  bind_rows(hist_tab, fcast_tab)
}

read_climate_forecasts <- function(main = ".", urls = NULL,
                                   raw_path_covfcast = "cov_fcasts",
                                   controls = list()){
  return_if_null(urls)
  controls <- do.call(climate_dl_controls, controls)
  dat_list <- vector("list", length(controls$data))
  ndatas <- length(controls$data)
  for(i in 1:ndatas){
    loc_path <- paste0("raw/", raw_path_covfcast, "/", names(urls)[i], ".csv")
    fpath <- file_paths(main, loc_path)
    dat_list[[i]] <- read.csv(fpath)
  }
  dat_tab <- dat_list[[1]]
  colnames(dat_tab)[ncol(dat_tab)] <- names(controls$data)[1]
  colnames(dat_tab)[1] <- "date"
  dat_tab[,1] <- as.Date(dat_tab[,1])
  dat_tab <- dat_tab[ , c(1, ncol(dat_tab))]
  
  if(ndatas > 1){
    for(i in 2:ndatas){
      dat_tab_i <- dat_list[[i]]
      x <- dat_tab_i[ , ncol(dat_tab_i)]
      dat_tab <- data.frame(dat_tab, x)
      colnames(dat_tab)[ncol(dat_tab)] <- names(controls$data)[i]
    }
  }
  for(i in 2:ncol(dat_tab)){
    if(grepl("temp", colnames(dat_tab)[i])){
      x <- dat_tab[ , i]
      x[x == -9999] <- NA
      dat_tab[ , i] <- (x - 32) * 5 / 9      
    } else if (grepl("precip", colnames(dat_tab)[i])){
      x <- dat_tab[ , i]
      x[x == -9999] <- NA
      x[x < 0] <- 0
      dat_tab[ , i] <- x * 25.4
    }
  }
  dat_tab
}


forecast_window <- function(moons = prep_moons(main = main), 
                            cast_date = Sys.Date(),
                            lead_time = 12, min_lag = 6){
  lagged_lead <- lead_time - min_lag
  moons0 <- moons[moons$newmoondate < cast_date, ]
  last_moon <- tail(moons0, 1)
  last_moon$newmoondate <- as.Date(last_moon$newmoondate)
  future_moons <- get_future_moons(moons0, num_future_moons = lead_time)
  start_day <- as.character(as.Date(last_moon$newmoondate) + 1)
  end_day <- future_moons$newmoondate[lead_time]
  list(start = start_day, end = end_day)
}















summarize_daily_fcast_weather_by_newmoon <- function(x){
  group_by(x, newmoonnumber) %>%
  summarize(date = max(date, na.rm = TRUE), 
            mintemp = min(mintemp, na.rm = TRUE), 
            maxtemp = max(maxtemp, na.rm = TRUE), 
            meantemp = mean(meantemp, na.rm = TRUE), 
            precipitation = sum(precipitation, na.rm = TRUE)) %>% 
  arrange(newmoonnumber) %>% 
  select(newmoonnumber, mintemp, maxtemp, meantemp, precipitation)
}

summarize_daily_hist_weather_by_newmoon <- function(x){
  group_by(x, newmoonnumber) %>%
  summarize(date = max(date, na.rm = TRUE), 
            mintemp = min(mintemp, na.rm = TRUE), 
            maxtemp = max(maxtemp, na.rm = TRUE), 
            meantemp = mean(meantemp, na.rm = TRUE), 
            precipitation = sum(precipitation, na.rm = TRUE), 
            locally_measured = all(locally_measured), 
            battery_low = all(battery_low, na.rm = TRUE)) %>% 
  arrange(newmoonnumber) %>% 
  select(newmoonnumber, date, mintemp, maxtemp, meantemp, precipitation, 
         locally_measured, battery_low) %>% 
  mutate(battery_low = ifelse(date < "2003-01-01", NA, battery_low)) %>% 
  select(-c(.data$locally_measured, .data$battery_low, date))
}


add_newmoons_from_date <- function(x, moons){
  newmoon_number <- moons$newmoonnumber[-1]
  newmoon_start <- as.Date(moons$newmoondate[-nrow(moons)])
  newmoon_end <- as.Date(moons$newmoondate[-1])
  newmoon_match_number <- NULL
  newmoon_match_date <- NULL

  for (i in seq(newmoon_number)) {
    temp_dates <- seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1)
    temp_dates <- as.character(temp_dates)
    temp_numbers <- rep(newmoon_number[i], length(temp_dates))
    newmoon_match_date <- c(newmoon_match_date, temp_dates)
    newmoon_match_number <- c(newmoon_match_number, temp_numbers)
  }
  newmoon_match_date <- as.Date(newmoon_match_date)
  matches <- match(x$date, newmoon_match_date)
  x$newmoonnumber <- newmoon_match_number[matches]
  x
}

add_date_from_components <- function(x){
  yrs <- x$year
  mns <- x$month
  dys <- x$day
  x$date <- as.Date(paste(yrs, mns, dys, sep = "-"))
  x
}

forecast_ndvi <- function(covariates, moons = prep_moons(), lead_time = 12,
                          min_lag = 6){
  lagged_lead <- lead_time - min_lag
  ndvi_data <- select(covariates, c("newmoonnumber", "ndvi"))
  fcast_ndvi(ndvi_data, "newmoon", lagged_lead, moons)
}

trim_moons_fcast <- function(moons = prep_moons(), target_newmoons = NULL){
  moons <- moons[, c("newmoonnumber", "newmoondate", "period", "censusdate")]
  fc_nms <- moons
  addl_fcast <- which(moons$newmoonnumber %in% target_newmoons)
  if (length(addl_fcast) > 0){
    fc_nms <- fc_nms[-addl_fcast, ]
  }
  fc_nms
}



#' @title Download North American Multi-Model Ensemble climate forecasts
#'
#' @description Given the details specified in \code{controls}, download
#'  climate forecasts from the
#'  \href{https://bit.ly/2MifqjM}{Northwest Knowledge Network}'s (NKN) at
#'  \code{https://bit.ly/2tCP8NX}{simple API} to the 
#'  \href{https://bit.ly/2Mdv8gd}{North American Multi-Model Ensemble} (NMME).
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param raw_path_covfcast \code{character} value of the path within the
#'  raw subdirectory to the folder where the downloads are saved.
#'
#' @param controls \code{list} of specifications for the download, which
#'  are sent to \code{\link{NMME_urls}} to create the specific URLs.
#'
#' @param start \code{Date} for the start of the forecast.
#'
#' @param end \code{Date} for the end of the forecast. 
#'
#' @param model \code{character} value of the model, one of \code{"ENSMEAN"},
#'  (Multi-Model Mean), \code{"CMC1"} (CMC1-CanCM3), \code{"CMC2"}
#'  (CMC2-CanCM4), \code{"CFCSv2"} (NCEP-CFSv2), \code{"GFDL"} (GFDL-CM2.1),
#'  \code{"GFDL-FLOR"} (GFDL-FLOR), or \code{"NCAR"} (NCAR-CCSM4). \cr \cr
#'  Presently can only take one value.
#'
#' @param lat,lon \code{numeric} latitude and longitude values used to 
#'  downscale the model.  \cr \cr
#'  Presently can only take one value for each.
#'
#' @param freq \code{character} value of the frequency of the data, can 
#'  be \code{"daily"} or \code{"XmonthAverage"}, where \code{"X"} is a
#'  number between \code{1} and \code{7}. \cr \cr
#'  Presently can only take one value.
#'
#' @param data \code{character} value of the type of data, one of 
#'  \code{"tasmin"} (minimum temperature),  \code{"tasmean"}
#'  (mean temperature), \code{"tasmax"} (mximum temperature), \code{"pr"}
#'  (precipitation), \code{"dps"} (dew point), \code{"rsds"}
#'  (shortwave radiation; sun intensity), \code{"was"} (wind speed).
#'
#' @return Named \code{character} vector of URLs, or \code{NULL} if
#'  \code{data}, \code{freq}, or \code{model} is \code{NULL}.
#'
#' @return 
#'  \code{download_climate_forecasts}: downloads the files and returns a
#'   \code{character} vector of URLs where the downloads came from,
#'   as generated by \code{\link{NMME_urls}}. \cr \cr
#'  \code{climate_dl_controls}: a \code{list} of control arguments to pass
#'   to \code{\link{NMME_urls}} to generate the URLs for downloading.
#'
#' @examples
#'  \donttest{
#'  create_dir()
#'  download_climate_forecasts()
#'  }
#'
#' @export
#'
download_climate_forecasts <- function(main = ".", 
                                       raw_path_covfcast = "cov_fcasts",
                                       controls = list()){

  controls <- do.call(climate_dl_controls, controls)
  urls <- do.call(NMME_urls, controls)
  return_if_null(urls)
  raw_path <- sub_paths(main, "raw")
  cov_fcasts_path <- paste0(raw_path, "/", raw_path_covfcast)
  create(cov_fcasts_path, "covariate forecast download")
  for(i in 1:length(controls$data)){
    dl_name <- paste0(raw_path_covfcast, "/", names(urls)[i], ".csv")
    download(dl_name, "url", urls[i], main = main)
  }
  urls
}

#' @rdname download_climate_forecasts
#'
#' @export
#'
climate_dl_controls <- function(start = Sys.Date(), end = Sys.Date(),
                                model = "ENSMEAN", lat = 31.9555, 
                                lon = -109.0744, freq = "daily",
                                data = c(mintemp = "tasmin", 
                                         meantemp = "tasmean", 
                                         maxtemp = "tasmax", 
                                         precipitation = "pr")){
  list(start = start, end = end, model = model, lat = lat, lon = lon, 
       freq = freq, data = data)
}