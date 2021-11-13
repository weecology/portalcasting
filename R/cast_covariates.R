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
prepare_forecast_covariates <- function (main      = ".", 
                                         lead_time = 12,
                                         cast_date = Sys.Date(), 
                                         settings  = directory_settings(), 
                                         quiet     = TRUE,
                                         verbose   = FALSE) {

  moons <- read_moons(main = main, settings = settings)

  last_moon <- last_moon(moons = moons, date = cast_date)

  end_moon <- ifnull(end_moon, last_moon)
  min_lag <- ifna(min_lag, 0)
  hist_cov <- ifnull(hist_cov, prep_hist_covariates(main = main,
                                                    quiet = quiet)) 
  if(last_moon == end_moon){

    win <- cast_window(main = main, moons = moons, cast_date = cast_date,
                       lead_time = lead_time, min_lag = min_lag)

    cast_cov <- read_climate_forecasts(main = main)


    win_vec <- seq(win$start, win$end, 1)
    win_length <- length(win_vec)
    ndvi_fit <- auto.arima(hist_cov$ndvi)
    ndvi_cast <- forecast(ndvi_fit, h = win_length)
    ndvi_cast <- as.numeric(ndvi_cast$mean)


    spots <- na.omit(match(win_vec, cast_cov$date))
    incl <- win_vec %in% cast_cov$date
    cast_cov$ndvi[spots] <- ndvi_cast[incl]

    cast_cov <- add_moons_from_date(df = cast_cov, moons = moons)
    cast_cov_tab <- summarize_daily_weather_by_moon(cast_cov)
    cast_cov$cast_moon <- end_moon

    cast_moon <- end_moon
    covariates_tab <- data.frame(cast_moon, cast_cov_tab)
    lagged_lead <- lead_time - min_lag

    out <- save_cast_cov_csv(main = main, moons = moons, end_moon = end_moon, 
                           cast_date = cast_date, cast_cov = cast_cov,
                           control_files = control_files,
                           quiet = quiet, verbose = verbose)
    
    out <- covariates_tab[1:lagged_lead, ]


  } else {

    target_moons <- target_moons(main = main, moons = moons,
                                 end_moon = end_moon, lead_time = lead_time, 
                                 date = cast_date)
    cov_cast <- read_covariate_casts(main = main, 
                                     control_files = control_files,
                                     quiet = quiet, verbose = verbose)

    target_in <- cov_cast$moon %in% target_moons
    origin_in <- cov_cast$cast_moon %in% end_moon 
    cov_cast2 <- cov_cast[which(target_in & origin_in), ]

    if(NROW(cov_cast2) == 0){
      stop("covariates not available for requested end moon and lead time",
           call. = FALSE)
    }
    most_recent <- max(cov_cast2$date_made)
    made_in <- cov_cast2$date_made == most_recent
    out <- cov_cast2[which(made_in), ]
    out$date_made <- NULL
    out$date <- NULL
  }

  out$cast_moon <- NULL
  out$source <- "cast"
  out
}




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
