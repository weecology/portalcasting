#' @title Prepare model metadata
#'
#' @description Set up the model metadata, primarily the forecast timeperiods
#'
#' @param rodents rodent data table
#'
#' @param covariates covariate data table
#'
#' @param forecast_date date of the forecast (i.e., today)
#'
#' @param filename_suffix "forecasts" or "hindcasts" 
#'
#' @param confidence_level confidence level used in summarizing model output
#'
#' @param lead_time number of newmoons forward to forecast abundances
#'
#' @return model metadata as a list
#'
#' @export
#'
prep_metadata <- function(rodents, covariates, forecast_date = Sys.Date(), 
                          filename_suffix = "forecasts",
                          confidence_level = 0.9, lead_time = 12){
 
  # prev = previous (i.e. the most recent)
  
  moons <- prep_moon_data(future = TRUE)

  covariates_fcasts <- which(covariates$source == "fcast")

  which_prev_newmoon <- max(which(moons$newmoondate < forecast_date))
  prev_newmoon <- moons$newmoonnumber[which_prev_newmoon]

  prev_rodent_pd_all <- tail(rodents$all, 1)$period
  prev_rodent_pd_control <- tail(rodents$control, 1)$period
  prev_rodent_pd <- max(prev_rodent_pd_all, prev_rodent_pd_control)
  which_prev_rodent_pd <- which(moons$period == prev_rodent_pd)
  prev_rodent_newmoon <- moons$newmoonnumber[which_prev_rodent_pd]
  prev_covar_newmoon <- tail(covariates, 1)$newmoonnumber

  first_fcast_covar_newmoon <- prev_covar_newmoon + 1
  first_fcast_rodent_newmoon <- prev_rodent_newmoon + 1
  last_fcast_newmoon <- prev_newmoon + lead_time

  rodent_fcast_newmoons <- first_fcast_rodent_newmoon:last_fcast_newmoon
  which_r_nms <- which(moons$newmoonnumber %in% rodent_fcast_newmoons)
  rodent_nm_dates <- moons$newmoondate[which_r_nms]
  rodent_fcast_months <- as.numeric(format(rodent_nm_dates, "%m"))
  rodent_fcast_years <- as.numeric(format(rodent_nm_dates, "%Y"))

  covar_fcast_newmoons <- first_fcast_covar_newmoon:last_fcast_newmoon
  which_c_nms <- which(moons$newmoonnumber %in% covar_fcast_newmoons)
  covar_nm_dates <- moons$newmoondate[which_c_nms]
  covar_fcast_months <- as.numeric(format(covar_nm_dates, "%m"))
  covar_fcast_years <- as.numeric(format(covar_nm_dates, "%Y"))

  out <-  list(filename_suffix = filename_suffix, 
               forecast_date = as.character(forecast_date), 
               covariate_forecast_newmoons = covar_fcast_newmoons, 
               covariate_forecast_months = covar_fcast_months, 
               covariate_forecast_years = covar_fcast_years,
               rodent_forecast_newmoons = rodent_fcast_newmoons, 
               rodent_forecast_months = rodent_fcast_months, 
               rodent_forecast_years = rodent_fcast_years,
               confidence_level = confidence_level)
  return(out)
}