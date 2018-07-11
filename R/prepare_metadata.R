#' @title Prepare model metadata
#'
#' @description Set up the model metadata, primarily the forecast timeperiods
#'
#' @param moons moons data table
#'
#' @param rodents rodent data table
#'
#' @param covariates covariate data table
#'
#' @param options_metadata metadata options list
#'
#' @return model metadata as a list
#'
#' @export
#'
prep_metadata <- function(moons = prep_moons(), rodents = prep_rodents(), 
                          covariates = prep_covariates(), 
                          options_metadata = metadata_options()){
 
  if (!options_metadata$quiet){
    message("Loading the metadata file into data subdirectory")
  }

  forecast_date <- options_metadata$fdate
  
  covariates_fcasts <- which(covariates$source == "fcast")
  if (options_metadata$cast_type == "forecasts"){
    which_last_newmoon <- max(which(moons$newmoondate < forecast_date))
    last_newmoon <- moons$newmoonnumber[which_last_newmoon]
  }
  if (options_metadata$cast_type == "hindcasts"){
    last_newmoon <- max(covariates$newmoonnumber[covariates$source == "hist"]) 
  }
  last_rodent_pd_all <- tail(rodents$all, 1)$period
  last_rodent_pd_control <- tail(rodents$control, 1)$period
  last_rodent_pd <- max(last_rodent_pd_all, last_rodent_pd_control)
  which_last_rodent_pd <- which(moons$period == last_rodent_pd)
  last_rodent_newmoon <- moons$newmoonnumber[which_last_rodent_pd]
  last_covar_newmoon <- tail(covariates, 1)$newmoonnumber

  first_fcast_covar_newmoon <- last_covar_newmoon + 1
  first_fcast_rodent_newmoon <- last_rodent_newmoon + 1
  last_fcast_newmoon <- last_newmoon + options_metadata$lead_time

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

  out <-  list(filename_suffix = options_metadata$cast_type, 
               forecast_date = as.character(forecast_date), 
               covariate_forecast_newmoons = covar_fcast_newmoons, 
               covariate_forecast_months = covar_fcast_months, 
               covariate_forecast_years = covar_fcast_years,
               rodent_forecast_newmoons = rodent_fcast_newmoons, 
               rodent_forecast_months = rodent_fcast_months, 
               rodent_forecast_years = rodent_fcast_years,
               confidence_level = options_metadata$confidence_level)
  if (options_metadata$save){
    local_path <- paste0("data/", options_metadata$filename)
    con_path <- file_path(options_metadata$tree, local_path)
    writeLines(as.yaml(out), con = con_path)
  }
  return(out)
}