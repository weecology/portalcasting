#' @title Cast the covariates
#'
#' @description Cast the covariates for a model run. \cr \cr
#'  \code{cast_covariates} is the primary function, which produces the 
#'  covariates (min, mean, and max temperature; precipitation; and NDVI) 
#'  required for a model to be cast. \cr \cr
#'  \code{prep_cast_covariates} provides a wrapper on \code{cast_covariates}
#'  that includes saving the cast data out via \code{save_cast_cov_csv}
#'  and tidying the data for use within the model (by removing the vestigial
#'  columns that are only needed for saving). 
#'
#' @param main \code{character} name of the main directory tree component.
#'
#' @param moons Lunar data \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param hist_cov \code{data.frame} of historic covariates. See 
#'  \code{\link{prep_hist_covariates}}.
#'
#' @param cast_cov \code{data.frame} of cast covariates. See
#'  \code{\link{cast_covariates}}.
#'
#' @param lead_time \code{integer}-conformable number of timesteps forward 
#'  a cast will proceed.
#'
#' @param min_lag \code{integer}-conformable minimum non-0 covariate lag time 
#'  used in any model.
#'
#' @param cast_date \code{Date} on which the cast happens. Generally should
#'  be \code{\link{Sys.Date}}.
#'
#' @param end_moon Forecast origin. \code{integer}-conformable newmoon number 
#'  or \code{NULL}, which equates to the most recently included census'
#'  newmoon number. 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_climate_dl \code{list} of specifications for the climate 
#'  download. Sent to \code{\link{NMME_urls}} to create the specific URLs. See 
#'  \code{\link{climate_dl_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @details If forecasting from the current newmoon, weather is forecast using 
#'  \code{\link{cast_weather}} and NDVI is forecast using 
#'  \code{\link{cast_ndvi}}. If casting from a previous newmoon, the
#'  historical forecasted weather and NDVI data are retrieved from the file 
#'  pointed to by \code{filename_cov_casts}.
#'
#' @return 
#'  \code{cast_covariates}: \code{data.frame} of the covariates cast for the
#'   moons as defined as needed for saving. \cr \cr
#'  \code{prep_cast_covariates}: \code{data.frame} of the cast covariates
#'   after saving, with excess columns removed. \cr \cr
#'  \code{cast_ndvi}: \code{data.frame} of -casted NDVI values. \cr \cr
#'  \code{cast_weather}: \code{data.frame} of -casted weather values. \cr \cr
#'  \code{save_cast_cov_csv}: \code{cast_cov} \code{data.frame} exactly as 
#'   input. 
#'  
#' @examples
#'  \donttest{
#'   setup_dir()
#'   hist_cov <- prep_hist_covariates()
#'   prep_cast_covariates(hist_cov = hist_cov)
#'   cast_cov <- cast_covariates(hist_cov = hist_cov)
#'   save_cast_cov_csv(cast_cov = cast_cov)
#'   cast_ndvi_data <- cast_ndvi(hist_cov = hist_cov)
#'   cast_weather_data <- cast_weather(hist_cov = hist_cov)
#'  }
#'
#' @export
#'
cast_covariates <- function(main = ".", moons = NULL,
                            hist_cov = NULL, end_moon = NULL, lead_time = 12, 
                            min_lag = 6, cast_date = Sys.Date(), 
                            control_files = files_control(),
                            control_climate_dl = climate_dl_control(),
                            quiet = TRUE, verbose = FALSE, 
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  last_moon <- last_moon(main = main, moons = moons, date = cast_date, 
                         arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  min_lag <- ifna(min_lag, 0)
  hist_cov <- ifnull(hist_cov, prep_hist_covariates(main = main,
                                                    end_moon = end_moon,
                                                    quiet = quiet,
                                                    arg_checks = arg_checks)) 
  if(last_moon == end_moon){
    weather_cast <- cast_weather(main = main, moons = moons, 
                                 hist_cov = hist_cov, end_moon = end_moon,
                                 lead_time = lead_time, min_lag = min_lag,
                                 cast_date = cast_date, 
                                 control_climate_dl = control_climate_dl,
                                 control_files = control_files, 
                                 quiet = quiet,
                                 verbose = verbose, arg_checks = TRUE)
    ndvi_cast <- cast_ndvi(main = main, hist_cov = hist_cov,
                           lead_time = lead_time, min_lag = min_lag,
                           arg_checks = arg_checks)
    cov_cast <- right_join(weather_cast, ndvi_cast, by = "moon")
    cast_moon <- end_moon
    covariates_tab <- round(data.frame(cast_moon, cov_cast), 3)
    lagged_lead <- lead_time - min_lag
    out <- covariates_tab[1:lagged_lead, ]
  } else {
    target_moons <- target_moons(main = main, moons = moons,
                                 end_moon = end_moon, lead_time = lead_time, 
                                 date = cast_date, arg_checks = arg_checks)
    cov_cast <- read_covariate_casts(main = main, 
                                     control_files = control_files,
                                     quiet = quiet, verbose = verbose,
                                     arg_checks = arg_checks)

    target_in <- cov_cast$moon %in% target_moons
    origin_in <- cov_cast$cast_moon %in% end_moon 
    cov_cast2 <- cov_cast[which(target_in & origin_in), ]

    if(NROW(cov_cast2) == 0){
      stop("covariates not available for requested end moon and lead time")
    }
    most_recent <- max(cov_cast2$date_made)
    made_in <- cov_cast2$date_made == most_recent
    out <- cov_cast2[which(made_in), ]
    out <- select(out, -date_made)
  }
  out
}


#' @rdname cast_covariates
#'
#' @export
#'
prep_cast_covariates <- function(main = ".", moons = NULL,
                                 hist_cov = NULL, end_moon = NULL, 
                                 lead_time = 12, min_lag = 6, 
                                 cast_date = Sys.Date(), 
                                 control_climate_dl = climate_dl_control(),
                                 control_files = files_control(),
                                 quiet = TRUE, verbose = FALSE, 
                                 arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  cast_covariates(main = main, moons = moons, hist_cov = hist_cov, 
                  end_moon = end_moon, lead_time = lead_time, 
                  min_lag = min_lag, cast_date = cast_date, 
                  control_files = control_files,
                  control_climate_dl = control_climate_dl,
                  quiet = quiet, verbose = verbose,
                  arg_checks = arg_checks) %>%
  save_cast_cov_csv(main = main, moons = moons, end_moon = end_moon, 
                    cast_date = cast_date, .,
                    control_files = control_files,
                    quiet = quiet, verbose = verbose,
                    arg_checks = arg_checks) %>%
  select(-cast_moon) %>%
  mutate("source" = "cast")
}

#' @rdname cast_covariates
#'
#' @export
#'
cast_ndvi <- function(main = ".", hist_cov = NULL, lead_time = 12,
                      min_lag = 6, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  min_lag <- ifna(min_lag, 0)
  lagged_lead <- lead_time - min_lag
  ndvi_fit <- auto.arima(hist_cov$ndvi)
  ndvi_cast <- forecast(ndvi_fit, h = lagged_lead)
  max_moon <- max(hist_cov$moon)
  data.frame(ndvi = as.numeric(ndvi_cast$mean), moon = max_moon+(1:lead_time))
}

#' @rdname cast_covariates
#'
#' @export
#'
cast_weather <- function(main = ".", moons = NULL,
                         hist_cov = NULL, end_moon = NULL, lead_time = 12, 
                         min_lag = 6, cast_date = Sys.Date(), 
                         control_climate_dl = climate_dl_control(),
                         control_files = files_control(),
                         quiet = TRUE, verbose = TRUE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  target_moons <- target_moons(main = main, moons = moons,
                              end_moon = end_moon, lead_time = lead_time, 
                              date = cast_date, arg_checks = arg_checks)
  moons0 <- trim_moons(moons = moons, target_moons = target_moons, 
                       retain_target_moons = FALSE, arg_checks = arg_checks)
  raw_path <- raw_path(main = main, arg_checks = arg_checks)

  win <- cast_window(main = main, moons = moons, cast_date = cast_date,
                     lead_time = lead_time, min_lag = 6,                                
                     arg_checks = arg_checks)
  control_climate_dl <- do.call(climate_dl_control, control_climate_dl)
  control_climate_dl <- update_list(control_climate_dl, start = win$start, 
                                 end = win$end)

  download_climate_casts(main = main, control_climate_dl = control_climate_dl, 
                         quiet = quiet, verbose = verbose, 
                         arg_checks = arg_checks)

  weather_cast <- read_climate_casts(main = main, 
                                     control_climate_dl = control_climate_dl, 
                                     arg_checks = arg_checks)

  weather("daily", TRUE, raw_path) %>% 
  add_date_from_components(arg_checks = arg_checks) %>%
  select(-c(year, month, day, battery_low, locally_measured))  %>%
  combine_hist_and_cast(cast_tab = weather_cast, arg_checks = arg_checks) %>% 
  add_moons_from_date(moons = moons, arg_checks = arg_checks) %>%
  summarize_daily_weather_by_moon()
}


#' @rdname cast_covariates
#'
#' @export
#'
save_cast_cov_csv <- function(main = ".", moons = NULL,
                              end_moon = NULL, cast_date = Sys.Date(),
                              cast_cov = NULL, 
                              control_files = files_control(),
                              quiet = TRUE, verbose = FALSE, 
                              arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  return_if_null(cast_cov)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                         arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  if(!control_files$save | !control_files$append_cast_csv | 
     !control_files$overwrite | end_moon != last_moon){
    return(cast_cov)
  }
  new_cast <- cast_cov
  new_cast$source <- control_files$source_name
  date_made <- Sys.time()
  tz <- format(date_made, "%Z")
  new_cast$date_made <- paste0(date_made, " ", tz)

  hist_cast <- read_covariate_casts(main = main, 
                                    control_files = control_files,
                                    quiet = quiet, verbose = verbose, 
                                    arg_checks = arg_checks)
  out <- rbind(hist_cast, new_cast)
  out_path <- file_path(main = main, sub = "data", 
                        files = control_files$filename_cov_casts,
                        arg_checks = arg_checks)
  msg <- "    **covariates_casts.csv saved**"
  messageq(msg, !verbose)
  write.csv(out, out_path, row.names = FALSE)
  cast_cov
}

#' @title Download and read covariate casts (presently only the NMME)
#'
#' @description Given the details specified in \code{control_cdl}, download
#'  climate forecasts from the
#'  \href{https://bit.ly/2MifqjM}{Northwest Knowledge Network}'s (NKN) at
#'  \href{https://bit.ly/2tCP8NX}{simple API} to the 
#'  \href{https://bit.ly/2Mdv8gd}{North American Multi-Model Ensemble} (NMME)
#'  and them ready for analyses. \cr \cr
#'  \code{download_climate_casts}: downloads the files from the server.
#'  \cr \cr
#'  \code{read_climate_casts}: reads the downloaded files into R and does
#'  some minimal tidying: makes sure \code{-9999}s become \code{NA}s, 
#'  makes sure the \code{date} column is named as such and is actually a 
#'  \code{Date}, and converts the temperatures to C and the precipitation
#'  to mm. \cr \cr
#'  \code{climate_dl_control}: manages the control list for the URL generation
#'  via \code{\link{NMME_urls}}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param control_climate_dl \code{list} of specifications for the download, 
#'  which are sent to \code{\link{NMME_urls}} to create the specific URLs.
#'
#' @param start,end \code{Date} for the start and end of the cast.
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
#'  (mean temperature), \code{"tasmax"} (maximum temperature), \code{"pr"}
#'  (precipitation), \code{"dps"} (dew point), \code{"rsds"}
#'  (shortwave radiation; sun intensity), \code{"was"} (wind speed).
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @return Named \code{character} vector of URLs, or \code{NULL} if
#'  \code{data}, \code{freq}, or \code{model} is \code{NULL}.
#'
#' @return 
#'  \code{download_climate_casts}: downloads the files and returns a
#'   \code{character} vector of URLs where the downloads came from,
#'   as generated by \code{\link{NMME_urls}}. \cr \cr
#'  \code{read_climate_casts}: \code{data.frame} of the downloaded data
#'   formatted for casting. \cr \cr
#'  \code{climate_dl_control}: a \code{list} of control arguments to pass
#'   to \code{\link{NMME_urls}} to generate the URLs for downloading.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   download_climate_casts()
#'   read_climate_casts()
#'  }
#'
#' @export
#'
download_climate_casts <- function(main = ".", 
                                   control_climate_dl = climate_dl_control(), 
                                   quiet = TRUE, verbose = FALSE,
                                   arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  control_climate_dl <- do.call(climate_dl_control, control_climate_dl)
  urls <- do.call(NMME_urls, control_climate_dl)
  return_if_null(urls)
  cov_cast_path <- file_path(main = main, sub = "raw", files = "cov_casts",
                             arg_checks = arg_checks)
  create(cov_cast_path, "covariate cast download", quiet = quiet)

  for(i in 1:length(control_climate_dl$data)){
    dl_name <- paste0("cov_casts/", names(urls)[i])
    download(name = dl_name, type = "url", url = urls[i], main = main, 
             sep_char = "=", quiet = !verbose, return_version = FALSE,
             arg_checks = arg_checks, NULLname = TRUE)
  }
  urls
}

#' @rdname download_climate_casts
#'
#' @export
#'
climate_dl_control <- function(start = Sys.Date(), 
                               end = as.Date("2050-01-01"),
                               model = "ENSMEAN", lat = 31.9555, 
                               lon = -109.0744, freq = "daily",
                               data = c(mintemp = "tasmin", 
                                        meantemp = "tasmean", 
                                        maxtemp = "tasmax", 
                                        precipitation = "pr"), 
                                arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  list(start = start, end = end, model = model, lat = lat, lon = lon, 
       freq = freq, data = data)
}

#' @rdname download_climate_casts
#'
#' @export
#'
read_climate_casts <- function(main = ".", 
                               control_climate_dl = climate_dl_control(), 
                               arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  control_climate_dl <- do.call(climate_dl_control, control_climate_dl)
  urls <- do.call(NMME_urls, control_climate_dl)
  return_if_null(urls)

  dat_list <- vector("list", length(control_climate_dl$data))
  ndatas <- length(control_climate_dl$data)
  for(i in 1:ndatas){
    csv_path <- paste0("cov_casts/",  names(urls)[i], ".csv")
    fpath <- file_path(main = main, sub = "raw", files = csv_path, 
                       arg_checks = arg_checks)
    dat_list[[i]] <- read.csv(fpath)
  }
  dat_tab <- dat_list[[1]]
  colnames(dat_tab)[ncol(dat_tab)] <- names(control_climate_dl$data)[1]
  colnames(dat_tab)[1] <- "date"
  dat_tab[,1] <- as.Date(dat_tab[,1])
  dat_tab <- dat_tab[ , c(1, ncol(dat_tab))]
  
  if(ndatas > 1){
    for(i in 2:ndatas){
      dat_tab_i <- dat_list[[i]]
      x <- dat_tab_i[ , ncol(dat_tab_i)]
      dat_tab <- data.frame(dat_tab, x)
      colnames(dat_tab)[ncol(dat_tab)] <- names(control_climate_dl$data)[i]
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
