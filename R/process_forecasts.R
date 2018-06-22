#' @title Save forecast output to files
#'
#' @description Save out the forecast and AIC values from a given set of 
#'   forecasts on all and control-only rodent data
#'
#' @param all ouput from the forecasting function for all data
#'
#' @param controls ouput from the forecasting function for controls data
#'
#' @param name model name for saving in files
#'
#' @param metadata model metadata
#'
#' @param temp_dir directory name for the temporary housing of predictions
#'
#' @return Nothing
#'
#' @export
#'
save_forecast_output <- function(all, controls, name, metadata,
                                 temp_dir){
 
  forecasts <- rbind(all$forecast, controls$forecast)
  aics <- rbind(all$aic, controls$aic)

  fcast_fname <- paste0(name, metadata$filename_suffix, ".csv")
  fcast_path <- file.path(temp_dir, fcast_fname)
  write.csv(forecasts, fcast_path, row.names = FALSE)

  aic_fname <- paste0(name, metadata$filename_suffix, "_model_aic.csv")
  aic_path <- file.path(temp_dir, aic_fname)
  write.csv(aics, aic_path, row.names = FALSE)

}

#' @title Combine Forecasts and Append to Existing Files
#' 
#' @description Combine all new forecasts (from the tmp directory), add 
#'   append the results to the existing files
#' 
#' @param model_metadata model metadata list
#'
#' @param temp_dir directory name for the temporary housing of predictions
#'
#' @param pred_dir directory name where the saved model predictions reside
#'
#' @return list of [1] the forecasts and [2] the model AIC values
#'
#' @export
#'
combine_forecasts <- function(model_metadata, temp_dir,
                              pred_dir){
  
  forecast_date <- model_metadata$forecast_date
  filename_suffix <- model_metadata$filename_suffix
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  fcasts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))

  file_ptn <- paste(filename_suffix, "_model_aic.csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)

  aics <- do.call(rbind, lapply(files, read.csv, na.strings = ""))
  
  fcast_date <- as.character(forecast_date)
  fcast_fname <- paste(fcast_date, filename_suffix, ".csv", sep = "")
  forecast_filename <- file.path(pred_dir, fcast_fname)
  aic_fname <- paste(fcast_date, filename_suffix, "_model_aic.csv", sep = "")
  model_aic_filename <- file.path(pred_dir, aic_fname)
  append_csv(fcasts, forecast_filename)
  append_csv(aics, model_aic_filename)
  
  return(list(forecasts = fcasts, all_model_aic = aics))
}

#' @title Add Ensemble Model to Forecasts
#' 
#' @description Add ensembles to the forecast files
#' 
#' @param model_metadata model metadata list
#' 
#' @param temp_dir directory name for the temporary housing of predictions
#'
#' @param pred_dir directory name where the saved model predictions reside
#'
#' @return list of [1] the forecasts and [2] the model AIC values
#' 
#' @export
#'
add_ensemble <- function(model_metadata, temp_dir,
                         pred_dir){

  forecast_date <- model_metadata$forecast_date
  filename_suffix <- model_metadata$filename_suffix
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  fcasts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))
  fcast_date <- as.character(forecast_date)
  fcast_fname <- paste(fcast_date, filename_suffix, ".csv", sep = "")
  forecast_filename <- file.path(pred_dir, fcast_fname)

  ensemble <- make_ensemble(fcasts, pred_dir) %>% 
              subset(select = colnames(fcasts))
  append_csv(ensemble, forecast_filename)
  return(ensemble)
}

#' @title Calculate Model Weights
#' 
#' @description calculate akaike weights across the models
#' 
#' @param pred_dir directory name where the saved model predictions reside
#'
#' @return model weights
#' 
#' @export
#'
compile_aic_weights <- function(pred_dir){

  aic_files <- list.files(pred_dir, full.names = TRUE, recursive = TRUE)
  aic_files <- aic_files[grepl("model_aic", aic_files)]

  aics <- map(aic_files, 
            ~read.csv(.x, na.strings = "", stringsAsFactors = FALSE)) %>% 
          bind_rows()
 
  grps <- quos(date, currency, level, species, fit_start_newmoon, 
            fit_end_newmoon, initial_newmoon)
 
  wts <- aics %>%
         group_by(!!!grps) %>%
         mutate(delta_aic = aic - min(aic), 
                weight = exp(-0.5 * delta_aic) / sum(exp(-0.5*delta_aic))) %>%
         ungroup()

  return(wts)
}

#' @title Create the ensemble model from all other forecasts
#' 
#' @description Uses the weighted mean and weighted sample variance
#'   Mean is the weighted mean of all model means. Variance is the weighted 
#'   mean of all model variances + the variances of the weighted mean using
#'   the unbiased estimate of sample variance. See
#'   https://github.com/weecology/portalPredictions/pull/65
#'   We only store the prediction interval for models, so backcalculate 
#'   individual model variance assuming the same CI_level throughout. Assert
#'   that the summed weight of all the model ensembles is 1, as that's what
#'   the above variance estimates assume. Rounded to account for precision
#'   errors. Summed weights can also be NA if there are not weights availble
#'   for that ensemble. 
#' 
#' @param all_forecasts alll forecasts
#' 
#' @param pred_dir directory name where the saved model predictions reside
#'
#' @param models_to_use models to use
#' 
#' @param CI_level confidence interval level
#' 
#' @return ensemble
#' 
#' @export
#'
make_ensemble <- function(all_forecasts, pred_dir, models_to_use = NA,  
                          CI_level = 0.9){

  weights <- compile_aic_weights(pred_dir)
  weights$date <- as.Date(weights$date)
  CI_quantile <- qnorm((1 - CI_level) / 2, lower.tail = FALSE)

  leftj <- c("date", "model", "currency", "level", "species", 
             "fit_start_newmoon", "fit_end_newmoon", "initial_newmoon")
  grp <- quos(date, newmoonnumber, forecastmonth, forecastyear,level, 
           currency, species, fit_start_newmoon, fit_end_newmoon, 
           initial_newmoon)
  mod_var <- quo(((UpperPI - estimate)/CI_quantile) ^ 2)
  est <- quo(sum(estimate * weight))
  wtss <- quo(sum(weight * (estimate - ensemble_estimate)^2))
  ens_var <- quo(sum(model_var * weight) + 
                 weighted_ss / (n() * sum(weight) - 1))
  weighted_estimates <- all_forecasts %>%
                        mutate(model_var = !!mod_var) %>%
                        left_join(weights, by = leftj) %>%
                        group_by(!!!grp) %>%
                        summarise(ensemble_estimate = !!est, 
                                  weighted_ss = !!wtss ,
                                  ensemble_var = !!ens_var,
                                  sum_weight = sum(weight)) %>% 
                        ungroup() 
     
  check_sum <- round(weighted_estimates$sum_weight, 10) == 1 
  check_na <- is.na(weighted_estimates$sum_weight)       
  if(!all(check_sum | check_na)){ 
    stop("Summed weights do not equal 1")
  }

  PI_l <- quo(ensemble_estimate - (sqrt(ensemble_var) * CI_quantile))
  PI_U <- quo(ensemble_estimate + (sqrt(ensemble_var) * CI_quantile))
  ensemble <- weighted_estimates %>%
              mutate(LowerPI = !!PI_l, UpperPI = !!PI_U) %>%
              mutate(LowerPI = ifelse(LowerPI<0, 0, LowerPI)) %>%
              rename(estimate = ensemble_estimate) %>%
              select(-ensemble_var, -weighted_ss, -sum_weight)
  
  ensemble$model <- "Ensemble"
  return(ensemble)
}

