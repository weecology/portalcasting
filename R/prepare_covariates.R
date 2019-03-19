#' @title Prepare covariate data table
#'
#' @description Prepare covariate data for forecasting or hindcasting, 
#'   including saving out the data and appending the new forecasts of 
#'   covariates to the existing covariate forecast table.
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param options_covariates A class-\code{covariates_options} \code{list} of 
#'   settings controlling the covariates data creation.
#'
#' @return Covariate data table as a code{covariates}-class \code{data.frame}.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' prep_covariates()
#' }
#'
#' @export
#'
prep_covariates <- function(moons = prep_moons(),
                            options_covariates = covariates_options()){
  check_args()
  msg <- "Loading covariate data files into data subdirectory"
  messageq(msg, options_covariates$quiet)
  hist_cov <- prep_hist_covariates(options_covariates)

  if (options_covariates$cov_fcast){
    fcast_cov <- prep_fcast_covariates(hist_cov, moons, options_covariates)
  }
  out <- hist_cov[-(1:nrow(hist_cov)), ]
  if (options_covariates$cov_hist){
    out <- bind_rows(out, hist_cov)
  }
  if (options_covariates$cov_fcast){
    out <- bind_rows(out, fcast_cov)
  }
  dataout(out, options_covariates)
}

#' @title Transfer the historical covariate data forecasts to the data folder
#'
#' @description Currently, the historical covariate covariate forecasts are 
#'   held in the \code{extdata} folder in the package directory, and the
#'   transfer is hard-wired to come from there. This will be relaxed in the 
#'   future.
#'
#' @param options_data Class-\code{data_options} \code{list} of control 
#'   options \code{list}s for setting up the data structures.
#'
#' @export
#'
transfer_hist_covariate_forecasts <- function(options_data = data_options()){
  check_args()
  path_to <- file_paths(options_data$tree, "data/covariate_forecasts.csv")

  fname <- "extdata/covariate_forecasts.csv"
  path_from <- system.file(fname, package = "portalcasting")
  if (!file.exists(path_from)){
    stop("Historical covariate forecast data not found.")
  }
  temp <- read.csv(path_from, stringsAsFactors = FALSE)

  if (!file.exists(path_to)){
    msg <- "Loading historical covariate forecasts into data subdirectory"
    messageq(msg, options_data$quiet)
    write.csv(temp, path_to, row.names = FALSE)    
  } else{
    exists <- read.csv(path_to, stringsAsFactors = FALSE) 
    if (max(temp$date_made) > max(exists$date_made)){
      msg <- "Updating historical covariate forecasts"
      message(msg, options_data$quiet)
      write.csv(temp, path_to, row.names = FALSE)
    }
  }
}

#' @title Prepare historical covariates data
#'
#' @description Create a data table of historical weather and NDVI data.
#'
#' @param options_covariates A class-\code{covariates_options} \code{list} of 
#'   settings controlling the covariates data creation.
#'
#' @return Historical covariate data table as a code{covariates}-class 
#'   \code{data.frame}.
#'
#' @export
#'
prep_hist_covariates <- function(options_covariates = covariates_options()){
  check_args()
  tree <- options_covariates$tree
  weather_data <- prep_weather_data(tree = tree)
  ndvi_data <- ndvi("newmoon", fill = TRUE, path = main_path(tree))
  out <- right_join(weather_data, ndvi_data, by = "newmoonnumber")
  out$source <- "hist"
  if (!is.null(options_covariates$end)){
    end_step <- options_covariates$end[options_covariates$hind_step]
    out <- out[which(out$newmoonnumber <= end_step), ]
  }
  classy(out, c("covariates", "data.frame"))
}

#' @title Prepare forecast covariate data table
#'
#' @description Prepare forecasts of covariate data as needed for rodent
#'   forecasting, including appending the new forecasts of covariates to the
#'   existing covariate forecast table
#'
#' @param hist_cov Historical covariate data table as a code{covariates}-class 
#'   \code{data.frame}, returned from \code{\link{prep_hist_covariates}}.
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param options_covariates A class-\code{covariates_options} \code{list} of 
#'   settings controlling the covariates data creation.
#'
#' @return Covariate data table as a code{covariates}-class \code{data.frame}
#'   ready for casting.
#'
#' @export
#'
prep_fcast_covariates <- function(hist_cov = prep_hist_covariates(),
                                  moons = prep_moons(),
                                  options_covariates = covariates_options()){
  check_args()
  update_covfcast_options(options_covariates, hist_cov, moons) %>%
  forecast_covariates(hist_cov, moons, .) %>%
  append_cov_fcast_csv(options_covariates) %>%
  select(-forecast_newmoon) %>%
  mutate("source" = "fcast") %>%
  classy(class = c("covariates", "data.frame"))
}

#' @title Prepare historical weather data
#'
#' @description Create a data table of historical weather data using 
#'   \code{\link[portalr]{weather}}. 
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @return \code{data.frame} of historical weather data.
#'
#' @export
#'
prep_weather_data <- function(tree = dirtree()){
  check_args()
  cols <- c("mintemp", "maxtemp", "meantemp", "precipitation", 
            "newmoonnumber")
  weather("newmoon", fill = TRUE, path = main_path(tree)) %>% 
  ungroup() %>%
  select(cols) %>%
  remove_incompletes("newmoonnumber")
}


#' @title Update the data options for covariate forecasts based on existing 
#'   data
#'
#' @description Update the covariate data control options based on the 
#'   historical covariate data and moon data.
#'
#' @param hist_cov Historical covariate data table as a code{covariates}-class 
#'   \code{data.frame}, returned from \code{\link{prep_hist_covariates}}.
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param options_covariates A class-\code{covariates_options} \code{list} of 
#'   settings controlling the covariates data creation.
#'
#' @return An updated \code{covariates_options} \code{list} of control options 
#'   for covariates.
#'
#' @export
#'
update_covfcast_options <- function(options_covariates, hist_cov, moons){
  check_args()
  prev_newmoon <- max(which(moons$newmoondate < options_covariates$cast_date))
  prev_newmoon <- moons$newmoonnumber[prev_newmoon]
  if (options_covariates$cast_type == "hindcasts"){
    prev_newmoon <- options_covariates$end[options_covariates$hind_step]    
  }
  prev_covar_newmoon <- tail(hist_cov, 1)$newmoonnumber
  first_fcast_newmoon <- prev_covar_newmoon + 1
  last_fcast_newmoon <- prev_newmoon + options_covariates$lead_time
  options_covariates$fcast_nms <- first_fcast_newmoon:last_fcast_newmoon
  options_covariates$nfcnm <- length(options_covariates$fcast_nms)
  options_covariates
}


