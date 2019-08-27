#' @title Prepare covariate data for casting
#'
#' @description Combine the historical and forecast or hindcast covariate 
#'  data for a model run. See \code{link{prep_hist_covariates}} and
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
#' @param hist_covariates \code{logical} indicator of whether or not 
#'  historical covariates are to be included.
#'
#' @param cast_covariates \code{logical} indicator whether or not -casted 
#'   covariates are to be included.
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
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param control_cdl \code{list} of specifications for the download, which
#'  are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param filename_cov \code{character} filename for saving the output.
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
prep_covariates <- function(main = ".", moons = NULL,
                            end_moon = NULL, lead_time = 12, min_lag = 6, 
                            cast_date = Sys.Date(),
                            hist_covariates = TRUE, cast_covariates = TRUE,
                            raw_path_archive = "portalPredictions",
                            raw_cov_cast_file = "data/covariate_casts.csv",
                            raw_path_cov_cast = "cov_casts", 
                            source_name = "current_archive",
                            append_cast_csv = TRUE, 
                            control_cdl = climate_dl_control(), quiet = TRUE, 
                            verbose = FALSE, save = TRUE, overwrite = TRUE,
                            filename_cov = "covariates.csv", 
                            arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  messageq("  -covariate data files", quiet)
  hist_cov <- prep_hist_covariates(main = main, end_moon = end_moon,
                                   quiet = quiet, arg_checks = arg_checks)
  if (cast_covariates){
    cast_cov <- prep_cast_covariates(main = main, moons = moons, 
                                     hist_cov = hist_cov,
                                     end_moon = end_moon, 
                                     lead_time = lead_time, min_lag = min_lag, 
                                     cast_date = cast_date, 
                                     raw_path_archive = raw_path_archive,
                                     raw_cov_cast_file = raw_cov_cast_file,
                                     raw_path_cov_cast = raw_path_cov_cast, 
                                     source_name = source_name,
                                     append_cast_csv = append_cast_csv, 
                                     control_cdl = control_cdl,
                                     quiet = quiet, verbose = verbose, 
                                     save = save, overwrite = overwrite, 
                                     arg_checks = arg_checks)
  }
  out <- hist_cov[-(1:nrow(hist_cov)), ]
  if (hist_covariates){
    out <- bind_rows(out, hist_cov)
  }
  if (cast_covariates){
    out <- bind_rows(out, cast_cov)
  }
  data_out(out, main, save, filename_cov, overwrite, !verbose)
}


#' @title Prepare historical covariates data
#'
#' @description 
#'  \code{prep_hist_covariates} creates a \code{data.frame} of historical 
#'  weather and NDVI data. Automatically goes all the way back to the 
#'  beginning of the available data, as the table is automatically trimmed 
#'  later. \cr \cr
#'  \code{prep_weather_data} creates a \code{data.frame} of historical 
#'   weather data using \code{\link[portalr]{weather}}. \cr \cr
#'  NDVI data are generated by \code{\link[portalr]{ndvi}}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
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
#'  \code{prep_hist_covariates}: historical covariate data table as a 
#'   \code{data.frame}. \cr \cr
#'  \code{prep_weather_data}: \code{data.frame} of historical weather data.
#'
#' @examples
#'  \donttest{   
#'   create_dir()
#'   fill_raw()
#'   prep_hist_covariates()
#'   prep_weather_data()
#'  }
#'
#' @export
#'
prep_hist_covariates <- function(main = ".", end_moon = NULL, 
                                 quiet = TRUE, arg_checks = TRUE){
  check_args(arg_checks)
  weather_data <- prep_weather_data(main)
  raw_path <- sub_paths(main, "raw")
  ndvi_data <- ndvi("newmoon", TRUE, raw_path)
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  out$source <- "hist"
  colnames(out)[which(colnames(out) == "newmoonnumber")] <- "moon"
  if (!is.null(end_moon)){
    out <- out[which(out$moon <= end_moon), ]
  }
  data.frame(out)
}

#' @rdname prep_hist_covariates
#'
#' @export
#'
prep_weather_data <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks)
  cols <- c("mintemp", "maxtemp", "meantemp", "precipitation", 
            "newmoonnumber")
  raw_path <- sub_paths(main, "raw")
  weather("newmoon", TRUE, raw_path) %>% 
  ungroup() %>%
  select(cols) %>%
  remove_incompletes("newmoonnumber")
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
#'   "%>%" <- dplyr::"%>%"
#'   raw_path <- sub_paths(specific_subs = "raw")
#'   moons <- prep_moons()
#'   portalr::weather("daily", TRUE, raw_path) %>% 
#'   add_date_from_components() %>%
#'   dplyr::select(-c(year, month, day, battery_low, locally_measured))  %>%
#'   add_newmoons_from_date(moons) %>%
#'   summarize_daily_weather_by_newmoon()
#'  }
#'
#' @export
#'
summarize_daily_weather_by_moon <- function(x){
  group_by(x, moon) %>%
  summarize(date = max(date, na.rm = TRUE), 
            mintemp = min(mintemp, na.rm = TRUE), 
            maxtemp = max(maxtemp, na.rm = TRUE), 
            meantemp = mean(meantemp, na.rm = TRUE), 
            precipitation = sum(precipitation, na.rm = TRUE)) %>% 
  arrange(moon) %>% 
  select(moon, mintemp, maxtemp, meantemp, precipitation)
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
  check_args(arg_checks)
  covariates$moon_lag <- covariates$moon + lag
  
  if(tail == FALSE){
    oldest_included_moon <- covariates$moon[1]
    most_recent_moon <- covariates$moon[nrow(covariates)]
    hist_moons <- oldest_included_moon:most_recent_moon
    hist_moons_table <- data.frame(moon = hist_moons)
    nm_match <- c("moon_lag" = "moon")
    data <- right_join(covariates, hist_moons_table, by = nm_match) 
    if (lag > 0){
      covariates <- covariates[-(1:lag), ]
    }
  }
  covariates <- select(covariates, -moon)
  cn_covariates <- colnames(covariates)
  cn_nmn_l <- which(cn_covariates == "moon_lag")
  colnames(covariates)[cn_nmn_l] <- "moon"
  covariates
}
