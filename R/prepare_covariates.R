#' @title Prepare covariate data for casting
#'
#' @description Combine the historical and cast covariate data for a model
#'  run. \cr \cr See \code{\link{prep_hist_covariates}} and
#'  \code{\link{prep_cast_covariates}} for the historical and -casted
#'  covariate preparation details, respectively.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
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
#' @param control_climate_dl \code{list} of specifications for the download, 
#'  which are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @return \code{data.frame} of historical and -casted covariates, combined
#'  and saved out to \code{filename} if indicated by \code{save}.
#'  
#' @examples
#'  \donttest{
#'   setup_dir()
#'   prep_covariates()
#'  }
#'  
#' @export
#'
prep_covariates <- function(main = ".", moons = NULL, end_moon = NULL, 
                            start_moon = 217, lead_time = 12, min_lag = 6, 
                            cast_date = Sys.Date(),
                            control_climate_dl = climate_dl_control(), 
                            control_files = files_control(),
                            quiet = TRUE, verbose = FALSE, 
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  messageq("  -covariate data files", quiet)

  hist_cov <- prep_hist_covariates(main = main, moons = moons, quiet = quiet, 
                                   arg_checks = arg_checks)

  cast_cov <- prep_cast_covariates(main = main, moons = moons, 
                                   hist_cov = hist_cov,
                                   end_moon = end_moon, 
                                   lead_time = lead_time, min_lag = min_lag, 
                                   cast_date = cast_date, 
                                   control_files = control_files,
                                   control_climate_dl = control_climate_dl,
                                   quiet = quiet, verbose = verbose, 
                                   arg_checks = arg_checks)
  out <- combine_hist_and_cast(hist_tab = hist_cov, cast_tab = cast_cov, 
                               column = "moon", arg_checks = arg_checks)

  na_rows <- apply(is.na(out), 1, sum) > 0
  moon_in <- out$moon >= start_moon
  which_na_rows <- which(na_rows & moon_in)
  nna_rows <- length(which_na_rows)
  if(nna_rows > 0){

    cov_casts <- read_covariate_casts(main = main, 
                                      control_files = control_files, 
                                      arg_checks = arg_checks)
    for(i in 1:nna_rows){
      na_moon <- out$moon[which_na_rows[i]]
      possibles <- cov_casts[cov_casts$moon == na_moon, ]

      if(NROW(possibles > 0)){
        which_possible <- which.max(as.Date(possibles$date_made))
        cols_keep <- c("mintemp", "maxtemp", "meantemp", "precipitation",
                       "ndvi")
        patch <- data.frame(moon = na_moon, 
                            possibles[which_possible, cols_keep],
                            source = "cast")
        out[which_na_rows[i], ] <- patch
      }
    }
  }

  write_data(dfl = out, main = main, save = control_files$save, 
             filename = control_files$filename_cov, 
             overwrite = control_files$overwrite, 
             quiet = !verbose, arg_checks = arg_checks)
}


#' @title Prepare historical covariates data
#'
#' @description 
#'  \code{prep_hist_covariates} creates a \code{data.frame} of historical 
#'  weather and NDVI data. Automatically goes all the way back to the 
#'  beginning of the available data, as the table is automatically trimmed 
#'  later. \cr \cr
#'  NDVI data are generated by \code{\link[portalr]{ndvi}}.
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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

#' @rdname prepare_historical_covariates
#'
#' @export
#'
prep_hist_covariates <- function(main = ".", moons = NULL,
                                 control_files = files_control(),
                                 quiet = TRUE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))
  raw_path <- raw_path(main = main, arg_checks = arg_checks)
  weather_data <- weather(level = "daily", fill = TRUE, path = raw_path)
  ndvi_data <- ndvi(level = "daily", fill = TRUE, path = raw_path)
  ndvi_data$date <- as.Date(ndvi_data$date)

  out <- weather_data
  out$ndvi <- NA
  spots <- na.omit(match(ndvi_data$date, weather_data$date))
  incl <- ndvi_data$date %in% weather_data$date
  out$ndvi[spots] <- ndvi_data$ndvi[incl]
  out <- add_moons_from_date(df = out, moons = moons, arg_checks = arg_checks)
  cols_in <- c("date", "moon", 
               "mintemp", "maxtemp", "meantemp", "precipitation", "ndvi")
  out <- out[ , colnames(out) %in% cols_in]
  out <- summarize_daily_weather_by_moon(out)
  out$source <- "hist"
  data.frame(out)
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
lag_covariates <- function(covariates, lag, tail = FALSE, 
                           arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
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
