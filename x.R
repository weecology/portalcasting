
head(covariates)



  last_covar_moon <- max(c(covariates$moon, covariates$moon),
                         na.rm = TRUE)
  first_cast_covar_moon <- last_covar_moon + 1
  first_cast_rodent_moon <- last_rodent_moon + 1
  last_cast_moon <- end_moon + lead_time
  rodent_cast_moons <- first_cast_rodent_moon:last_cast_moon
  which_r_nms <- which(moons$moon %in% rodent_cast_moons)
  rodent_nm_dates <- as.Date(moons$moondate[which_r_nms])
  rodent_cast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_cast_years <- as.numeric(format(rodent_nm_dates, "%Y"))

  covar_cast_moons <- first_cast_covar_moon:last_cast_moon
  which_c_nms <- which(moons$moon %in% covar_cast_moons)
  covar_nm_dates <- as.Date(moons$moondate[which_c_nms])
  covar_cast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_cast_years <- as.numeric(format(covar_nm_dates, "%Y"))

  cast_type <- ifelse(end_moon == last_moon, "forecast", "hindcast")

  cast_meta <- read_casts_metadata(main = main, quiet = quiet)
  cast_group <- max(cast_meta$cast_group) + 1


  filename_config <- settings$files$directory_config
  filename_meta <- settings$files$metadata
           cast_group              = cast_group, 


           cast_type               = cast_type, 
           start_moon              = start_moon, 
           end_moon                = end_moon, 
           last_moon               = last_moon, 
           lead_time               = lead_time, 
           origin                  = as.character(origin), 
           covariate_cast_moons    = covar_cast_moons, 
           covariate_cast_months   = covar_cast_months, 
           covariate_cast_years    = covar_cast_years,
           rodent_cast_moons       = rodent_cast_moons, 
           rodent_cast_months      = rodent_cast_months, 
           rodent_cast_years       = rodent_cast_years,
           confidence_level        = settings$confidence_level

#' @title Determine specific newmoon numbers
#'
#' @description 
#'  \code{target_moons} determines which moons are in a window, based on a
#'  date and window width. \cr \cr
#'  \code{last_moon} determines the most recently passed newmoon, based on
#'  a date.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param date \code{Date} used for defining a window (starting with moons
#'  subsequent to the date) or a previous moon (before or on the date).
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return 
#'  \code{target_moons}: \code{numeric} vector of the moon numbers 
#'  targeted by the date window.
#'  \code{last_newmoon}:  \code{numeric} value of the last moon number.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   target_moons()
#'   last_moon()
#'  }
#'
#' @export
#'
target_moons <- function(moons = NULL, end_moon = NULL, 
                         lead_time = 12, date = Sys.Date()){

  return_if_null(moons)

  end_moon <- ifnull(end_moon, last_moon(moons = moons, date = date))

  (end_moon + 1):(end_moon + lead_time)

}
main = main, models = models,
                datasets = datasets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_model = controls_model,
                quiet = quiet, 
                control_files = control_files)

#' @rdname target_moons
#'
#' @export
#'
last_moon <- function(moons = NULL, date = Sys.Date()){

  return_if_null(moons)
  
  moons$newmoonnumber[max(which(moons$newmoondate < date))]

}
#' @title Trim a moons table based on target moons
#'
#' @description Some functions require that a data table of time (the moons)
#'  table only includes specific time stamps (newmoon numbers). This function
#'  is a simple utility to reduce the moons table to that as well as the 
#'  columns.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param target_moons \code{integer}-conformable newmoon numbers, usually
#'  made through \code{\link{target_moons}} of the moons to either 
#'  drop or retain based on \code{retain_target_moons}. 
#' 
#' @param retain_target_moons \code{logical} value that dictates if 
#'  \code{target_moons} are to be dropped (\code{retain_target_moons = FALSE}
#'  or retained (\code{retain_target_moons = TRUE})
#'
#' @param target_cols \code{character} vector of columns to retain.
#'
#' @return \code{moons} and a \code{data.frame} but trimmed.
#' 
#' @examples
#'  \donttest{
#'   setup_dir()
#'   moons <- prep_moons()
#'   trim_moons(moons, 300:310)
#'  }
#'
#' @export
#'
trim_moons <- function(moons = NULL, target_moons = NULL, 
                       retain_target_moons = TRUE,
                       target_cols = c("moon", "moondate", 
                                       "period", "censusdate")){

  return_if_null(moons)
  moons <- moons[, target_cols]
  new_moons <- moons
  which_target_moons <- which(moons$moon %in% target_moons)
  ntarget_moons <- length(which_target_moons)
  if (ntarget_moons > 0){
    if(retain_target_moons){
      new_moons <- new_moons[which_target_moons, ]
    } else{
      new_moons <- new_moons[-which_target_moons, ]
    }
  }
  new_moons
}



#' @title Summarize a daily weather table by newmoons
#'
#' @description Summarizes a daily weather table by newmoons. Taking the 
#'  max date, min of the min temperatures, max of the max temperatures,
#'  mean of the mean temperatures, and sum of the precipitation.
#'
#' @param x \code{data.frame} of daily weather, with columns named
#'  \code{"date"}, \code{"mintemp"}, \code{"maxtemp"}, \code{"meantemp"}, 
#'  \code{"precipitation"}, and \code{"moon"}.
#'
#' @return \code{data.frame} of \code{x} summarized and arranged by
#'  \code{x$moon}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_raw()
#'   raw_path <- raw_path()
#'   moons <- prep_moons()
#'   weather <- portalr::weather("daily", TRUE, raw_path)
#'   weather <- add_date_from_components(weather)
#'   weather <- add_moons_from_date(weather, moons)
#'   summarize_daily_weather_by_moon(weather)
#'  }
#'
#' @export
#'
summarize_daily_weather_by_moon <- function(x){

  x <- x[!(is.na(x$moon)), ] 
  umoons <- unique(x$moon)
  numoons <- length(umoons)
  date <- rep(NA, numoons)
  mintemp <- rep(NA, numoons)
  maxtemp <- rep(NA, numoons)
  meantemp <- rep(NA, numoons)
  precipitation <- rep(NA, numoons)
  ndvi <- rep(NA, numoons)
  for(i in 1:numoons){
    moons_in <- x$moon == umoons[i]
    date[i] <- max(as.Date(x$date[moons_in], na.rm = TRUE))
    mm <- na.omit(x$mintemp[moons_in])
    if(length(mm) > 0){
      mintemp[i] <- min(mm, na.rm = TRUE)
    } else{
      mintemp[i] <- NA
    }

    mm <- na.omit(x$maxtemp[moons_in])
    if(length(mm) > 0){
      maxtemp[i] <- max(mm, na.rm = TRUE)
    } else{
      maxtemp[i] <- NA
    }

    mm <- na.omit(x$meantemp[moons_in])
    if(length(mm) > 0){
      meantemp[i] <- mean(mm, na.rm = TRUE)
    } else{
      meantemp[i] <- NA
    }

    pp <- na.omit(x$precipitation[moons_in])
    if(length(mm) > 0){
      precipitation[i] <- sum(pp, na.rm = TRUE)
    } else{
      precipitation[i] <- NA
    }

    nn <- na.omit(x$ndvi[moons_in])
    if(length(nn) > 0){
      ndvi[i] <- sum(nn, na.rm = TRUE)
    } else{
      ndvi[i] <- NA
    }

  }
  out <- data.frame(moon = umoons, mintemp, maxtemp, meantemp, precipitation,
                    ndvi)
  moon_order <- order(out$moon)
  out[moon_order, ]
}



#' @title Lag covariate data
#'
#' @description Lag the covariate data together based on the new moons
#'
#' @param covariates \code{data.frame} of covariate data to be lagged. 
#'  
#' @param lag \code{integer} lag between rodent census and covariate data, in
#'  new moons.
#'  
#' @param tail \code{logical} indicator if the data lagged to the tail end 
#'  should be retained.
#'
#' @return \code{data.frame} with a \code{newmoonnumber} column reflecting
#'  the lag.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   covariate_casts <- read_covariate_casts()
#'   covar_casts_lag <- lag_covariates(covariate_casts, lag = 2, tail = TRUE)
#'  }
#'
#' @export
#'
lag_covariates <- function(covariates, lag, tail = FALSE){

  covariates$moon_lag <- covariates$moon + lag
  
  if(tail == FALSE){
    oldest_included_moon <- covariates$moon[1]
    most_recent_moon <- covariates$moon[nrow(covariates)]
    hist_moons <- oldest_included_moon:most_recent_moon
    moon_match <- match(covariates$moon_lag, hist_moons)
    covariates$moon <- hist_moons[moon_match]
    if (lag > 0){
      covariates <- covariates[-(1:lag), ]
    }
  }
  covariates$moon <- covariates$moon_lag
  covariates$moon_lag <- NULL
  covariates
}

#' @title Cast the covariates
#'
#' @description Cast the covariates for a model run. \cr \cr
#'  \code{prep_cast_covariates} is the primary function, which produces the 
#'  covariates (min, mean, and max temperature; precipitation; and NDVI) 
#'  required for a model to be cast. Saves the cast data out via
#'  \code{save_cast_cov_csv} and tidying the data for use within the model 
#'  (by removing the vestigial columns that are only needed for saving). 
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
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @details If forecasting from the current newmoon, casts the covariates 
#'  If casting from a previous newmoon, the
#'  historical forecasted weather and NDVI data are retrieved from the file 
#'  pointed to by \code{filename_cov_casts}.
#'
#' @return 
#'  \code{prep_cast_covariates}: \code{data.frame} of the cast covariates
#'   after saving, with excess columns removed. \cr \cr
#'  \code{save_cast_cov_csv}: \code{cast_cov} \code{data.frame} exactly as 
#'   input. 
#'  
#' @examples
#'  \donttest{
#'   setup_dir()
#'   hist_cov <- prep_hist_covariates()
#'   cast_cov <- prep_cast_covariates(hist_cov = hist_cov)
#'   save_cast_cov_csv(cast_cov = cast_cov)
#'  }
#'
#' @name cast_covariates
#'




#' @rdname cast_covariates
#'
#' @export
#'
save_cast_cov_csv <- function(main = ".", moons = NULL,
                              end_moon = NULL, cast_date = Sys.Date(),
                              cast_cov = NULL, 
                              control_files = files_control(),
                              quiet = TRUE, verbose = FALSE){

  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files))
  return_if_null(cast_cov)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date)
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

  arch_path <- paste0(control_files$directory, "/data/", 
                      control_files$filename_cov_casts)
  arch_path <- file_path(main = main, sub = "raw", files = arch_path)
  arch_path2 <- gsub("covariate_casts", "covariate_forecasts", arch_path)

  if (file.exists(arch_path) | file.exists(arch_path2)) {
    hist_cast <- read_covariate_casts(main = main, 
                                      control_files = control_files,
                                      quiet = quiet, verbose = verbose)
    if(!("date" %in% colnames(hist_cast))){
      hist_cast$date <- NA
    }
  
    out <- rbind(hist_cast, new_cast)

  } else {

   out <- new_cast

  }

  out$date <- as.character(out$date)
  out_path <- file_path(main = main, sub = "data", 
                        files = control_files$filename_cov_casts)
  msg <- "    **covariates_casts.csv saved**"
  messageq(msg, quiet = !verbose)
  write.csv(out, out_path, row.names = FALSE)
  cast_cov
}




                                   min_lag = min_lag, 


#
# patching NDVI 
#

  na_ndvi <- is.na(out$ndvi)

  if (na_ndvi[nrow(out)]) {

    last_good <- max(which(!na_ndvi))
    full_length <- nrow(out)
    window_length <- full_length - last_good

    ndvi_fit <- auto.arima(out$ndvi)
    ndvi_cast <- forecast(ndvi_fit, h = window_length)
    out$ndvi[(last_good + 1):full_length] <- as.numeric(ndvi_cast$mean)

  }

  data.frame(out)


 moons = data_m, end_moon = end_moon, 
                            start_moon = start_moon, lead_time = lead_time, 
                            min_lag = min_lag, cast_date = cast_date, 
                            quiet = quiet, control_files = control_files)

#' @title Prepare historical covariates data
#'
#' @description 

#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param moons Lunar data \code{data.frame}. See \code{\link{prep_moons}}.
#'
#'
#' @return 
#'  \code{prep_hist_covariates}: daily historical covariate data table as a 
#'   \code{data.frame}. 
#'
#' @examples
#'  \donttest{   
#'   create_dir()
#'   fill_raw()
#'   prep_hist_covariates()
#'  }
#'
#' @name prepare_historical_covariates
#'
NULL


#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
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
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.


\code{list} of datasets to be created using \code{\link{do.call}} on the defined functions. 

#'              If the requested data do not exist, an effort is made to prepare them using the associated \code{prep_<data_name>} functions (like \code{prep_moons}).\cr \cr

names(prefab_rodent_datasets())
names(prefab_rodent_datasets())

 main = ".", models = prefab_models(),
                          datasets = NULL, moons = NULL, rodents = NULL,
                          covariates = NULL, end_moon = NULL, 
                          start_moon = 217, lead_time = 12, min_lag = 6, 
                          cast_date = Sys.Date(), confidence_level = 0.95, 
                          controls_model = NULL, 
                          control_files = files_control(),
                          quiet = TRUE, verbose = FALSE
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param rodents Rodents \code{list}. See \code{\link{prep_rodents}},
#'  
#' @param datasets \code{character} vector of the rodent data set names
#'  that the model is applied to. 
#'
#' @param covariates Covariates \code{data.frame}. See 
#'  \code{\link{prep_covariates}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
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
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param controls_model Additional controls for models not in the prefab
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.




lead_time = 12, cast_date = Sys.Date(), 
                       control_files = files_control()


            end_moon = end_moon,
            start_moon = start_moon,
            lead_time = lead_time, min_lag = min_lag,
            confidence_level = confidence_level,
            cast_date = cast_date,
                      controls_model = controls_model,
                      control_files = control_files
)

#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
#'
#' @param PortalData_source,PortalData_version \code{character} values for the
#'        source and version of the Portal Data to download. 
#'        \cr \cr 
#'        Default values retrieve the latest data from github.
#'        \cr \cr 
#'        See \code{\link[portalr]{download_observations}}.
#'
#' @param portalPredictions_source,portalPredictions_version \code{character} 
#'        values for the source and version of the archive to download.
#'        \cr \cr 
#'        Default values point to github, but \code{version = NULL} indicates
#'        no download.
#'        \cr \cr 
#'        See \code{\link{download_archive}}.

#'
#'
#' @param climate_forecast_source,climate_forecast_version  \code{character} 
#'        values for the source and version of the climate forecasts to
#'        download.
#'        \cr \cr 
#'        Default values retrieve the current day's forecast from the
#'        Northwest Knowledge Network's North American Multi-Model Ensemble 
#'        (NMME) climate forecasts.
#'        \cr \cr 
#'        See \code{\link{download_climate_forecasts}}.
#'
#' @param models \code{vector} of models to be written via 
#'        \code{\link{write_model}}.
#'
#' @param datasets \code{list} of datasets to be created using 
#'        \code{\link{do.call}} on the defined functions. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{datasets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{datasets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.


(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_model = NULL,

                          control_files = files_control(),
                      datasets = prefab_rodent_datasets(),
                          quiet = FALSE, verbose = TRUE){






#' @rdname setup_dir
#'
#' @export
#'
setup_production <- function(main = ".", models = prefab_models(), 
                             end_moon = NULL, start_moon = 217, 
                             lead_time = 12, confidence_level = 0.95, 
                             cast_date = Sys.Date(), controls_model = NULL,

                             control_files = files_control(), 
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = "latest",
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                             quiet = FALSE, verbose = TRUE, 
                      datasets = prefab_rodent_datasets()){
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
            quiet = quiet, verbose = verbose, 
                      datasets = datasets,
                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
            control_files = control_files)
}


#' @rdname setup_dir
#'
#' @export
#'
setup_sandbox <- function(main = ".", models = prefab_models(), 
                          end_moon = NULL, start_moon = 217, lead_time = 12, 
                          confidence_level = 0.95, cast_date = Sys.Date(), 
                          controls_model = NULL,

                          control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      datasets = prefab_rodent_datasets(),
                          quiet = FALSE, verbose = TRUE){
  setup_dir(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_model = controls_model, 
                             PortalData_version = PortalData_version,
                      datasets = datasets,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
quiet = quiet, verbose = verbose, 
            control_files = control_files)
  sandbox_welcome(main = main, quiet = quiet)
}


                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 climate_forecast_source = climate_forecast_source,
                 climate_forecast_version = climate_forecast_version,
               models = models, 
                      datasets = datasets,
           controls_model = controls_model, 
           control_files = control_files, 

           end_moon = end_moon, lead_time = lead_time, 
           cast_date = cast_date, start_moon = start_moon, 
           confidence_level = confidence_level, 
           quiet = quiet, verbose = verbose)



end_moon = NULL, 
                      start_moon = 217, lead_time = 12, 
                      confidence_level = 0.95, cast_date = Sys.Date(),
                      controls_model = NULL, 
                      control_files = files_control(),
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),

#'
_dir <- function (main  = ".",
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      models = prefab_models(),
                      datasets = prefab_rodent_datasets(),
                      quiet = FALSE,
                      verbose = TRUE,
                        controls_model = NULL, 
                        control_files = files_control(), 
                        end_moon = NULL, start_moon = 217, lead_time = 12,
 min_lag = 6, 
                        confidence_level = 0.95, cast_date = Sys.Date())
                        
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param controls_model Additional controls for models not in the prefab
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 


#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param filename \code{character} value of the path to the directory config YAML.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param downloads_versions \code{character} vector returned from \code{\link{fill_raw}} of the successfully downloaded versions of downloaded data.


files_control <- function(directory = "portalPredictions",
                          raw_data = "PortalData",
                          filename_moons = ,
                          filename_config = "dir_config.yaml", 
                          filename_cov = "covariates.csv", 
                          filename_cov_casts = "covariate_casts.csv",
                          filename_meta = "metadata.yaml", 
                          save = TRUE, overwrite = TRUE, cleanup = TRUE,
                          source_name = "current_archive",
                          append_cast_csv = TRUE){
  
  list(directory = directory, raw_data = raw_data, 
       filename_moons = filename_moons, filename_cov = filename_cov,
       filename_cov_casts = filename_cov_casts,
       filename_config = filename_config, filename_meta = filename_meta,
       save = save, overwrite = overwrite, cleanup = cleanup,
       source_name = source_name, append_cast_csv = append_cast_csv)

}


#'
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'                  data directory. A standard portalcasting directory
#'                  downloads the raw data files into from the PortalData 
#'                  repository, so \code{raw_data = "PortalData"}.

#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'

#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts later use.
#'
#' @param source \code{character} value for the name to give the covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
