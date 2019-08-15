#' @title Cast the covariates
#'
#' @description Forecast or hindcast the covariates for a model run.
#'  \code{cast_covariates} is the primary function, which produces the 
#'  coavariates (min, mean, and max temperature; precipitation; and NDVI) 
#'  required for a model to be forecast or hindcast. \cr \cr
#'  \code{prep_cast_covariates} provides a wrapper on \code{cast_covariates}
#'  that includes saving the cast data out via \code{save_cast_cov_csv}
#'  and tidying the data for use within the model (by removing the vestigial
#'  columns that are only needed for saving). 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param hist_cov \code{data.frame} of historic covariates from 
#'  \code{\link{prep_hist_covariates}}.
#'
#' @param cast_cov \code{data.frame} of cast covariates from 
#'  \code{\link{cast_covariates}}.
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param cast_type \code{character} value of the -cast type: 
#'  \code{"forecasts"} or \code{"hindcasts"}.
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
#'   covariate forecast. Currently is \code{"current_archive"}. Previous to
#'   \code{"current_archive"}, the data were retroactively filled in and are 
#'   given the source name \code{"retroactive"}.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'   cast covariates should be appended to the historical casts for the
#'   purposes of hindcasting later.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_cdl \code{list} of specifications for the 
#'  climate download, which are sent to \code{\link{NMME_urls}} to create the 
#'  specific URLs. See \code{\link{climate_dl_control}}.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @details If \code{cast_type = "forecast"}, weather is forecast using 
#'  \code{\link{cast_weather}} and NDVI is forecast using 
#'  \code{\link{cast_ndvi}}. If \code{cast_type = "hindcast"}, the historical
#'  forecasted weather and NDVI data are retrieved from the file pointed to
#'  by \code{raw_cov_cast_file}.
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
#'  
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
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
cast_covariates <- function(main = ".", moons = prep_moons(main = main),
                            hist_cov = NULL, end_moon = NULL, lead_time = 12, 
                            min_lag = 6, cast_date = Sys.Date(), 
                            cast_type = "forecast",
                            raw_path_archive = "portalPredictions",
                            raw_cov_cast_file = "data/covariate_casts.csv",
                            raw_path_cov_cast = "cov_casts", 
                            source_name = "current_archive",
                            control_cdl = list(), quiet = FALSE){

  if(cast_type == "forecast"){
    weather_cast <- pass_and_call(cast_weather)
    ndvi_cast <- pass_and_call(cast_ndvi)
    cov_cast <- right_join(weather_cast, ndvi_cast, by = "newmoonnumber")
    which_cast_newmoon <- max(which(moons$newmoondate < cast_date))
    cast_newmoon <- moons$newmoonnumber[which_cast_newmoon]
    out <- round(data.frame(cast_newmoon, cov_cast), 3)
  } else if (cast_type == "hindcast"){
    target_moons <- pass_and_call(target_newmoons)
    lpath <- paste0("raw/", raw_path_archive, "/", raw_cov_cast_file)
    pth <- file_paths(main, lpath)
    if(!file.exists(pth)){
      pth <- gsub("covariate_casts", "covariate_forecasts", pth)
      cov_cast <- read.csv(pth, stringsAsFactors = FALSE)
      which_old <- which(colnames(cov_cast) == "forecast_newmoon")
      colnames(cov_cast)[which_old] <- "cast_newmoon"
    } else{
      cov_cast <- read.csv(pth, stringsAsFactors = FALSE)
    }
    target_in <- cov_cast$newmoonnumber %in% target_moons
    origin_in <- cov_cast$forecast_newmoon %in% end_moon 
    out <- select(cov_cast[which(target_in & origin_in), ], -date_made)
  } else {
    stop("cast_type can only be forecast or hindcast")
  }
  out
}

#' @rdname cast_covariates
#'
#' @export
#'
prep_cast_covariates <- function(main = ".", moons = prep_moons(main = main),
                                 hist_cov = NULL, end_moon = NULL, 
                                 lead_time = 12, min_lag = 6, 
                                 cast_date = Sys.Date(), 
                                 cast_type = "forecast",
                                 raw_path_archive = "portalPredictions",
                                 raw_cov_cast_file = 
                                   "data/covariate_casts.csv",
                                 raw_path_cov_cast = "cov_casts", 
                                 source_name = "current_archive",
                                 append_cast_csv = TRUE, 
                                 control_cdl = list(),
                                 quiet = FALSE, save = TRUE, 
                                 overwrite = TRUE){
  cast_cov <- pass_and_call(cast_covariates)
  pass_and_call(save_cast_cov_csv, cast_cov = cast_cov)
  select(cast_cov, -cast_newmoon) %>%
  mutate("source" = "cast")
}

#' @rdname cast_covariates
#'
#' @export
#'
cast_ndvi <- function(main = ".", moons = prep_moons(main = main), 
                      hist_cov = NULL, lead_time = 12, min_lag = 6){
  lagged_lead <- lead_time - min_lag
  ndvi_data <- select(hist_cov, c("newmoonnumber", "ndvi"))
  fcast_ndvi(ndvi_data, "newmoon", lagged_lead, moons)
}

#' @rdname cast_covariates
#'
#' @export
#'
cast_weather <- function(main = ".", moons = prep_moons(main = main),
                         hist_cov = NULL, end_moon = NULL, lead_time = 12, 
                         min_lag = 6, cast_date = Sys.Date(), 
                         cast_type = "forecast",
                         raw_path_archive = "portalPredictions",
                         raw_cov_cast_file = "data/covariate_casts.csv",
                         raw_path_cov_cast = "cov_casts", 
                         source_name = "current_archive",
                         control_cdl = list(), quiet = FALSE){
  target_moons <- pass_and_call(target_newmoons)
  moons0 <- trim_moons(moons, target_moons, retain_target_moons = FALSE)
  raw_path <- sub_paths(main, "raw")
  win <- pass_and_call(cast_window)
  control_cdl <- update_list(control_cdl, start = win$start, end = win$end)
  pass_and_call(download_climate_casts, control_cdl = control_cdl)
  weather_cast <- pass_and_call(read_climate_casts, control_cdl = control_cdl)

  weather("daily", TRUE, raw_path) %>% 
  add_date_from_components() %>%
  select(-c(year, month, day, battery_low, locally_measured))  %>%
  combine_hist_and_cast(weather_cast) %>% 
  add_newmoons_from_date(moons) %>%
  summarize_daily_weather_by_newmoon()
}

#' @rdname cast_covariates
#'
#' @export
#'
save_cast_cov_csv <- function(main = ".",
                              cast_cov = NULL, cast_type = "forecast",
                              append_cast_csv = TRUE,
                              raw_path_archive = "portalPredictions",
                              raw_cov_cast_file = "data/covariate_casts.csv",
                              source_name = "current_archive",
                              quiet = FALSE, save = TRUE, overwrite = TRUE){
  return_if_null(cast_cov)
  if(!save | !append_cast_csv | !overwrite | cast_type == "hindcast"){
    return(cast_cov)
  }
  new_cast <- cast_cov
  new_cast$source <- source_name
  date_made <- Sys.time()
  tz <- format(date_made, "%Z")
  new_cast$date_made <- paste0(date_made, " ", tz)
  hist_file <- file_paths(main, raw_cov_cast_file)
  if(file.exists(hist_file)){
    msg <- "covariates_casts.csv exists and overwrite = TRUE; file saved"
    hist_cast <- read.csv(hist_file, stringsAsFactors = FALSE)
    out <- rbind(hist_cast, new_cast)
  } else{
    msg <- "covariates_casts.csv does not exist already; file saved"
    lpath <- paste0("raw/", raw_path_archive, "/", raw_cov_cast_file)
    dl_hist_file <- file_paths(main, lpath)
    if(file.exists(dl_hist_file)){
      hist_cast <- read.csv(dl_hist_file, stringsAsFactors = FALSE)
      out <- rbind(hist_cast, new_cast)
    } else {
      out <- new_cast
    }      
  }
  messageq(msg, quiet)
  write.csv(out, hist_file, row.names = FALSE)
  cast_cov
}

#' @title Download and read covariate casts (presently only the NMME)
#'
#' @description Given the details specified in \code{control_cdl}, download
#'  climate forecasts from the
#'  \href{https://bit.ly/2MifqjM}{Northwest Knowledge Network}'s (NKN) at
#'  \code{https://bit.ly/2tCP8NX}{simple API} to the 
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
#'  (mean temperature), \code{"tasmax"} (mximum temperature), \code{"pr"}
#'  (precipitation), \code{"dps"} (dew point), \code{"rsds"}
#'  (shortwave radiation; sun intensity), \code{"was"} (wind speed).
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
#'  create_dir()
#'  download_climate_casts()
#'  read_climate_casts()
#'  }
#'
#' @export
#'
download_climate_casts <- function(main = ".", 
                                   raw_path_cov_cast = "cov_casts",
                                   control_cdl = list()){

  control_cdl <- do.call(climate_dl_control, control_cdl)
  urls <- do.call(NMME_urls, control_cdl)
  return_if_null(urls)
  raw_path <- sub_paths(main, "raw")
  cov_cast_path <- paste0(raw_path, "/", raw_path_cov_cast)
  create(cov_cast_path, "covariate cast download")
  for(i in 1:length(control_cdl$data)){
    dl_name <- paste0(raw_path_cov_cast, "/", names(urls)[i], ".csv")
    download(dl_name, "url", urls[i], main = main)
  }
  urls
}

#' @rdname download_climate_casts
#'
#' @export
#'
climate_dl_control <- function(start = Sys.Date(), end = Sys.Date(),
                                model = "ENSMEAN", lat = 31.9555, 
                                lon = -109.0744, freq = "daily",
                                data = c(mintemp = "tasmin", 
                                         meantemp = "tasmean", 
                                         maxtemp = "tasmax", 
                                         precipitation = "pr")){
  list(start = start, end = end, model = model, lat = lat, lon = lon, 
       freq = freq, data = data)
}

#' @rdname download_climate_casts
#'
#' @export
#'
read_climate_casts <- function(main = ".", raw_path_cov_cast = "cov_casts",
                               control_cdl = list()){
  control_cdl <- do.call(climate_dl_control, control_cdl)
  urls <- do.call(NMME_urls, control_cdl)
  return_if_null(urls)

  dat_list <- vector("list", length(control_cdl$data))
  ndatas <- length(control_cdl$data)
  for(i in 1:ndatas){
    csv_path <- paste0(raw_path_cov_cast, "/", names(urls)[i], ".csv")
    raw_path <- sub_paths(main, "raw")
    fpath <- normalizePath(paste0(raw_path, "/", csv_path), mustWork = FALSE)
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
