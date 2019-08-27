#' @title Cast the covariates
#'
#' @description Forecast or hindcast the covariates for a model run.
#'  \code{cast_covariates} is the primary function, which produces the 
#'  coavariates (min, mean, and max temperature; precipitation; and NDVI) 
#'  required for a model to be forecast or hindcast. \cr \cr
#'  \code{prep_cast_covariates} provides a wrapper on \code{cast_covariates}
#'  that includes saving the cast data out via \code{save_cast_cov_csv}
#'  and tidying the data for use within the model (by removing the vestigial
#'  columns that are only needed for saving). \cr \cr
#'  \code{read_cov_casts} reads in the current or (if no current version)
#'  local archived version of the covariate casts, or downloads it, then
#'  reads it in if it is missing.
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
#' @param raw_path_archive \code{character} value of the path to the archive
#'  within the raw sub folder.
#' 
#' @param raw_cov_cast_file \code{character} value of the path to the
#'  covariate cast file within \code{raw_path_archive}.
#'
#' @param raw_path_cov_cast \code{character} value of the path to the folder
#'  that holds any downloaded covariate cast files within the raw sub folder.
#'
#' @param source_name \code{character} value for the name to give the 
#'  covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts for the
#'  purposes of hindcasting later.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_cdl \code{list} of specifications for the climate download.
#'  Sent to \code{\link{NMME_urls}} to create the specific URLs. See 
#'  \code{\link{climate_dl_control}}.
#'
#' @param overwrite \code{logical} indicator of whether existing
#'  files should be updated if \code{save = TRUE} (most users should leave as 
#"  \code{TRUE}).
#'
#' @param save \code{logical} indicator controlling if the output should 
#'  be saved.
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
#'  \code{\link{cast_ndvi}}. If hindcasting from a previous newmoon, the
#'  historical forecasted weather and NDVI data are retrieved from the file 
#'  pointed to by \code{raw_cov_cast_file}.
#'
#' @return 
#'  \code{cast_covariates}: \code{data.frame} of the covariates cast for the
#'   moons as defined as needed for saving. \cr \cr
#'  \code{prep_cast_covariates}: \code{data.frame} of the cast covariates
#'   after saving, with excess columns removed. \cr \cr
#'  \code{cast_ndvi}: \code{data.frame} of -casted NDVI values. \cr \cr
#'  \code{cast_weather}: \code{data.frame} of -casted weather values. \cr \cr
#'  \code{save_cast_cov_csv}: \code{cast_cov} \code{data.frame} exactly as 
#'   input. \cr \cr
#'  \code{read_cov_casts}: \code{data.frame} of the covariate casts read in.
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
#'   read_cov_casts()
#'  }
#'
#' @export
#'
cast_covariates <- function(main = ".", moons = NULL,
                            hist_cov = NULL, end_moon = NULL, lead_time = 12, 
                            min_lag = 6, cast_date = Sys.Date(), 
                            raw_path_archive = "portalPredictions",
                            raw_cov_cast_file = "data/covariate_casts.csv",
                            raw_path_cov_cast = "cov_casts", 
                            source_name = "current_archive",
                            control_cdl = climate_dl_control(),
                            quiet = TRUE, verbose = FALSE, 
                            arg_checks = TRUE){
  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main))
  last_moon <- last_moon(main, moons, cast_date, arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  if(last_moon == end_moon){
    weather_cast <- cast_weather(main = main, moons = moons, 
                                 hist_cov = hist_cov, end_moon = end_moon,
                                 lead_time = lead_time, min_lag = min_lag,
                                 cast_date = cast_date, 
                                 raw_path_archive = raw_path_archive,
                                 raw_cov_cast_file = raw_cov_cast_file,
                                 raw_path_cov_cast = raw_path_cov_cast, 
                                 source_name = source_name,
                                 control_cdl = control_cdl, quiet = quiet,
                                 verbose = verbose, arg_checks = TRUE)
    ndvi_cast <- cast_ndvi(main = main, moons = moons, hist_cov = hist_cov,
                           lead_time = lead_time, min_lag = min_lag,
                           arg_checks = arg_checks)
    cov_cast <- right_join(weather_cast, ndvi_cast, by = "moon")
    which_cast_moon <- max(which(moons$moon < cast_date))
    cast_moon <- moons$moon[which_cast_moon]
    out <- round(data.frame(cast_moon, cov_cast), 3)
  } else {
    target_moons <- target_moons(main = main, moons = moons,
                                    end_moon = end_moon, 
                                    lead_time = lead_time, 
                                    date = cast_date,
                                    arg_checks = arg_checks)
    cov_cast <- read_cov_casts(main = main, 
                               raw_path_archive = raw_path_archive,
                               raw_cov_cast_file = raw_cov_cast_file,
                               raw_path_cov_cast = raw_path_cov_cast, 
                               quiet = quiet, arg_checks = arg_checks)

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
read_cov_casts <- function(main = ".", raw_path_archive = "portalPredictions",
                           raw_cov_cast_file = "data/covariate_casts.csv",
                           raw_path_cov_cast = "cov_casts", quiet = FALSE,
                           arg_checks = TRUE){
  check_args(arg_checks)
  curr_path <- paste0("/", raw_cov_cast_file)
  curr_path <- file_paths(main, curr_path)
  curr_path2 <- gsub("covariate_casts", "covariate_forecasts", curr_path)

  arch_path <- paste0("raw/", raw_path_archive, "/", raw_cov_cast_file)
  arch_path <- file_paths(main, arch_path)
  arch_path2 <- gsub("covariate_casts", "covariate_forecasts", arch_path)

  if(file.exists(curr_path)){
    cov_cast <- read.csv(curr_path, stringsAsFactors = FALSE)
  } else if (file.exists(curr_path2)){
    cov_cast <- read.csv(curr_path2, stringsAsFactors = FALSE)
  } else {
    if(file.exists(arch_path)){
      cov_cast <- read.csv(arch_path, stringsAsFactors = FALSE)
    } else if (file.exists(arch_path2)){
      cov_cast <- read.csv(arch_path2, stringsAsFactors = FALSE)
    } else {
      messageq("current and archive versions missing, downloading archive")
      faulty_arch <- file_paths(main, paste0("raw/", raw_path_archive))
      unlink(faulty_arch, recursive = TRUE)
      fill_raw(main = main, downloads = zenodo_downloads("833438"),
              quiet = quiet, arg_checks = arg_checks)
      cov_cast <- read_cov_casts(main = main, 
                                 raw_path_archive = raw_path_archive,
                                 raw_cov_cast_file = raw_cov_cast_file,
                                 raw_path_cov_cast = raw_path_cov_cast, 
                                 quiet = quiet, arg_checks = arg_checks)
    }
  }
  if(any(grepl("forecast_newmoon", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("forecast_newmoon", "cast_moon", 
                                colnames(cov_cast))
  }
  if(any(grepl("newmoonnumber", colnames(cov_cast)))){
    colnames(cov_cast) <- gsub("newmoonnumber", "moon", colnames(cov_cast))
  } 
  cov_cast
}

#' @rdname cast_covariates
#'
#' @export
#'
prep_cast_covariates <- function(main = ".", moons = NULL,
                                 hist_cov = NULL, end_moon = NULL, 
                                 lead_time = 12, min_lag = 6, 
                                 cast_date = Sys.Date(), 
                                 raw_path_archive = "portalPredictions",
                                 raw_cov_cast_file = 
                                   "data/covariate_casts.csv",
                                 raw_path_cov_cast = "cov_casts", 
                                 source_name = "current_archive",
                                 append_cast_csv = TRUE, 
                                 control_cdl = climate_dl_control(),
                                 quiet = TRUE, verbose = FALSE, save = TRUE, 
                                 overwrite = TRUE, arg_checks = TRUE){

  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main))
  cast_covariates(main = main, moons = moons, hist_cov = hist_cov, 
                  end_moon = end_moon, lead_time = lead_time, 
                  min_lag = min_lag, cast_date = cast_date, 
                  raw_path_archive = raw_path_archive,
                  raw_cov_cast_file = raw_cov_cast_file,
                  raw_path_cov_cast = raw_path_cov_cast, 
                  source_name = source_name, control_cdl = control_cdl,
                  quiet = quiet, verbose = verbose,
                  arg_checks = arg_checks) %>%
  save_cast_cov_csv(main = main, moons = moons, end_moon = end_moon, 
                    cast_date = cast_date, .,
                    append_cast_csv = append_cast_csv, 
                    raw_path_archive = raw_path_archive,
                    raw_cov_cast_file = raw_cov_cast_file,
                    source_name = source_name, quiet = quiet, 
                    verbose = verbose, save = save,
                    overwrite = overwrite, arg_checks = arg_checks) %>%
  select(-cast_moon) %>%
  mutate("source" = "cast")
}

#' @rdname cast_covariates
#'
#' @export
#'
cast_ndvi <- function(main = ".", moons = NULL, 
                      hist_cov = NULL, lead_time = 12, min_lag = 6,
                      arg_checks = TRUE){
  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main))
  min_lag <- ifna(min_lag, lead_time)
  lagged_lead <- lead_time - min_lag
  ndvi_data <- select(hist_cov, c("moon", "ndvi"))
  moonsx <- moons
  colnames(moonsx)[which(colnames(moonsx) == "moon")] <- "newmoonnumber"
  colnames(moonsx)[which(colnames(moonsx) == "moondate")] <- "newmoondate"
  colnames(ndvi_data)[which(colnames(ndvi_data) == "moon")] <- "newmoonnumber"
  out <- fcast_ndvi(ndvi_data, "newmoon", lagged_lead, moonsx)
  colnames(out)[which(colnames(out) == "newmoonnumber")] <- "moon"
  out
}

#' @rdname cast_covariates
#'
#' @export
#'
cast_weather <- function(main = ".", moons = NULL,
                         hist_cov = NULL, end_moon = NULL, lead_time = 12, 
                         min_lag = 6, cast_date = Sys.Date(), 
                         raw_path_archive = "portalPredictions",
                         raw_cov_cast_file = "data/covariate_casts.csv",
                         raw_path_cov_cast = "cov_casts", 
                         source_name = "current_archive",
                         control_cdl = climate_dl_control(), quiet = TRUE,
                         verbose = verbose, arg_checks = TRUE){
  check_args(arg_checks)
  moons <- ifnull(moons, read_moons(main = main))
  target_moons <- target_moons(main = main, moons = moons,
                              end_moon = end_moon, 
                              lead_time = lead_time, 
                              date = cast_date,
                              arg_checks = arg_checks)
  moons0 <- trim_moons(moons, target_moons, retain_target_moons = FALSE)
  raw_path <- sub_paths(main, "raw")

  win <- cast_window(main = main, moons = moons, cast_date = cast_date,
                     lead_time = lead_time, min_lag = 6,                                
                     arg_checks = arg_checks)
  control_cdl <- do.call(climate_dl_control, control_cdl)
  control_cdl <- update_list(control_cdl, start = win$start, end = win$end)

  download_climate_casts(main = main, raw_path_cov_cast = raw_path_cov_cast,
                         control_cdl = control_cdl, quiet = quiet,
                         verbose = verbose, arg_checks = arg_checks)

  weather_cast <- read_climate_casts(main = main, 
                                     raw_path_cov_cast = raw_path_cov_cast,
                                     control_cdl = control_cdl, 
                                     arg_checks = arg_checks)

  weather("daily", TRUE, raw_path) %>% 
  add_date_from_components() %>%
  select(-c(year, month, day, battery_low, locally_measured))  %>%
  combine_hist_and_cast(weather_cast) %>% 
  add_moons_from_date(moons) %>%
  summarize_daily_weather_by_moon()
}


#' @rdname cast_covariates
#'
#' @export
#'
save_cast_cov_csv <- function(main = ".", moons = NULL,
                              end_moon = NULL, cast_date = Sys.Date(),
                              cast_cov = NULL, append_cast_csv = TRUE,
                              raw_path_archive = "portalPredictions",
                              raw_cov_cast_file = "data/covariate_casts.csv",
                              raw_path_cov_cast = "cov_casts", 
                              source_name = "current_archive",
                              quiet = TRUE, verbose = FALSE, save = TRUE, 
                              overwrite = TRUE, 
                              nindent = 4, arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  return_if_null(cast_cov)
  which_last_moon <- max(which(moons$moondate < cast_date))
  last_moon <- moons$moon[which_last_moon]
  end_moon <- ifnull(end_moon, last_moon)

  if(!save | !append_cast_csv | !overwrite | end_moon != last_moon){
    return(cast_cov)
  }
  new_cast <- cast_cov
  new_cast$source <- source_name
  date_made <- Sys.time()
  tz <- format(date_made, "%Z")
  new_cast$date_made <- paste0(date_made, " ", tz)


  hist_cast <- read_cov_casts(main = main, 
                              raw_path_archive = raw_path_archive,
                              raw_cov_cast_file = raw_cov_cast_file,
                              raw_path_cov_cast = raw_path_cov_cast, 
                              quiet = quiet, arg_checks = arg_checks)
  out <- rbind(hist_cast, new_cast)
  out_path <- file_paths(main, raw_cov_cast_file)

  msg <- "**covariates_casts.csv saved**"
  msg <- paste0(paste(rep(" ", nindent), collapse = ""), msg)
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
#'  and them ready for analyese. \cr \cr
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
#' @param raw_path_cov_cast \code{character} value of the path within the
#'  raw subdirectory to the folder where the downloads are saved.
#'
#' @param control_cdl \code{list} of specifications for the download, which
#'  are sent to \code{\link{NMME_urls}} to create the specific URLs.
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
#'   checked using standard protocols via \code{\link{check_args}}. The 
#'   default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'   formatted correctly and provides directed error messages if not. \cr
#'   However, in sandboxing, it is often desirable to be able to deviate from 
#'   strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'   many/most/all enclosed functions to not check any arguments using 
#'   \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
                                   raw_path_cov_cast = "cov_casts",
                                   control_cdl = climate_dl_control(), 
                                   quiet = TRUE, verbose = FALSE,
                                   arg_checks = TRUE){
  check_args(arg_checks)
  control_cdl <- do.call(climate_dl_control, control_cdl)
  urls <- do.call(NMME_urls, control_cdl)
  return_if_null(urls)
  raw_path <- sub_paths(main, "raw")
  cov_cast_path <- paste0(raw_path, "/", raw_path_cov_cast)
  create(cov_cast_path, "covariate cast download", quiet = quiet)
  for(i in 1:length(control_cdl$data)){
    dl_name <- paste0(raw_path_cov_cast, "/", names(urls)[i])
    download(dl_name, "url", urls[i], main = main, sep_char = "=", 
             quiet = !verbose)
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
  check_args(arg_checks)
  list(start = start, end = end, model = model, lat = lat, lon = lon, 
       freq = freq, data = data)
}

#' @rdname download_climate_casts
#'
#' @export
#'
read_climate_casts <- function(main = ".", raw_path_cov_cast = "cov_casts",
                               control_cdl = climate_dl_control(), 
                               arg_checks = TRUE){
  check_args(arg_checks)
  control_cdl <- do.call(climate_dl_control, control_cdl)
  urls <- do.call(NMME_urls, control_cdl)
  return_if_null(urls)

  dat_list <- vector("list", length(control_cdl$data))
  ndatas <- length(control_cdl$data)
  for(i in 1:ndatas){
    csv_path <- paste0("raw/", raw_path_cov_cast, "/", names(urls)[i], ".csv")
    fpath <- file_paths(main, csv_path)
    dat_list[[i]] <- read.csv(fpath)
  }
  dat_tab <- dat_list[[1]]
  colnames(dat_tab)[ncol(dat_tab)] <- names(control_cdl$data)[1]
  colnames(dat_tab)[1] <- "date"
  dat_tab[,1] <- as.Date(dat_tab[,1])
  dat_tab <- dat_tab[ , c(1, ncol(dat_tab))]
  
  if(ndatas > 1){
    for(i in 2:ndatas){
      dat_tab_i <- dat_list[[i]]
      x <- dat_tab_i[ , ncol(dat_tab_i)]
      dat_tab <- data.frame(dat_tab, x)
      colnames(dat_tab)[ncol(dat_tab)] <- names(control_cdl$data)[i]
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
