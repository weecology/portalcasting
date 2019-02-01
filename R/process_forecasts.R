#' @title Save forecast output to files
#'
#' @description Save out the forecast abundances and AIC values from a given 
#'   set of forecasts on all and control-only rodent data.
#'
#' @param all Ouput from a forecasting model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "all" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param controls Ouput from a forecasting model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "controls" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param name \code{character} model name for saving in files.
#'
#' @param metadata Class-\code{metadata} model metadata \code{list}.
#'
#' @param temp_dir \code{character} of the path to the subdirectory 
#'   temporarily housing the predictions. Under default settings, this should 
#'   be \code{sub_path(dirtree(), "tmp")}.
#'
#' @export
#'
save_forecast_output <- function(all, controls, name, metadata,
                                 temp_dir){
  if (!("list" %in% class(all))){
    stop("`all` is not a list")
  } 
  if (!all(c("forecast", "aic") %in% names(all))){
    stop("`all` does not have elements named `forecast` and `aic`")
  } 
  if (!("list" %in% class(controls))){
    stop("`controls` is not a list")
  } 
  if (!all(c("forecast", "aic") %in% names(controls))){
    stop("`controls` does not have elements named `forecast` and `aic`")
  } 
  if (length(name) > 1){
    stop("`name` can only be of length = 1")
  }
  if (!is.character(name)){
    stop("`name` is not a character")
  }
  if (length(temp_dir) > 1){
    stop("`temp_dir` can only be of length = 1")
  }
  if (!is.character(temp_dir)){
    stop("`temp_dir` is not a character")
  }
  if (!("metadata" %in% class(metadata))){
    stop("`metadata` is not a metadata list")
  } 
  forecasts <- rbind(all$forecast, controls$forecast)
  aics <- rbind(all$aic, controls$aic)

  fcast_fname <- paste0(name, metadata$filename_suffix, ".csv")
  fcast_path <- file.path(temp_dir, fcast_fname)
  write.csv(forecasts, fcast_path, row.names = FALSE)

  aic_fname <- paste0(name, metadata$filename_suffix, "_model_aic.csv")
  aic_path <- file.path(temp_dir, aic_fname)
  write.csv(aics, aic_path, row.names = FALSE)

}

#' @title Combine forecasts and append them to existing files
#' 
#' @description Combine all new forecasts (from the tmp subdirectory) into
#'   and append the results to the existing files.
#' 
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @return \code{list} of [1] \code{"forecasts"} (the forecasted abundances)
#'   and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @export
#'
combine_forecasts <- function(options_cast = cast_options()){
  if (!("cast_options" %in% class(options_cast))){
    stop("`cast_options` not of class `options_cast`")
  }
  if (!options_cast$quiet){
    message(paste0("Compiling ", options_cast$cast_type))
  }
  temp_dir <- sub_path(options_cast$tree, "tmp")
  pred_dir <- sub_path(options_cast$tree, "predictions")
  forecast_date <- options_cast$fdate
  filename_suffix <- options_cast$cast_type
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
  
  list(forecasts = fcasts, all_model_aic = aics)
}

#' @title Add ensemble model to forecasts
#' 
#' @description Add the predictions of an ensemble model to the forecasting 
#'   files.
#' 
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @return \code{list} of [1] \code{"forecasts"} (the forecasted abundances)
#'   and [2] \code{"all_model_aic"} (the model AIC values).
#' 
#' @export
#'
add_ensemble <- function(options_cast = cast_options()){
  if (!("cast_options" %in% class(options_cast))){
    stop("`cast_options` not of class `options_cast`")
  }
  if (options_cast$ensemble){
    if (!options_cast$quiet){
      message("Creating ensemble model")
    }
    temp_dir <- sub_path(options_cast$tree, "tmp")
    pred_dir <- sub_path(options_cast$tree, "predictions")
    forecast_date <- options_cast$fdate
    filename_suffix <- options_cast$cast_type
    file_ptn <- paste(filename_suffix, ".csv", sep = "")
    files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
    cclass <- c("Date", "integer", "integer", "integer", "character", 
                "character", "character", "character", "numeric",
                "numeric", "numeric", "integer", "integer", "integer")
    fcasts <- do.call(rbind, 
              lapply(files, read.csv, na.strings = "", colClasses  = cclass))
    fcast_date <- as.character(forecast_date)
    fcast_fname <- paste(fcast_date, filename_suffix, ".csv", sep = "")
    forecast_filename <- file.path(pred_dir, fcast_fname)

    ensemble <- make_ensemble(fcasts, pred_dir) %>% 
                subset(select = colnames(fcasts))
    append_csv(ensemble, forecast_filename)
    return(ensemble)
  }
}

#' @title Calculate model weights
#' 
#' @description Calculate AIC-based weights across the models for the full
#'   time series.
#' 
#' @param pred_dir \code{character} value of the path to the directory where 
#'   the saved model predictions reside.
#'
#' @return \code{data.frame} of model weights.
#' 
#' @export
#'
compile_aic_weights <- function(pred_dir){
  if (length(pred_dir) > 1){
    stop("`pred_dir` can only be of length = 1")
  }
  if (!is.character(pred_dir)){
    stop("`pred_dir` is not a character")
  }
  aic_files <- list.files(pred_dir, full.names = TRUE, recursive = TRUE)
  aic_files <- aic_files[grepl("model_aic", aic_files)]

  aics <- map(aic_files, 
            ~read.csv(.x, na.strings = "", stringsAsFactors = FALSE)) %>% 
          bind_rows()
 
  grps <- quos(date, currency, level, species, fit_start_newmoon, 
            fit_end_newmoon, initial_newmoon)
 
  aics %>%
  group_by(!!!grps) %>%
  mutate(delta_aic = aic - min(aic), 
         weight = exp(-0.5 * delta_aic) / sum(exp(-0.5*delta_aic))) %>%
  ungroup()
}

#' @title Create the ensemble model from all other forecasts
#' 
#' @description Combine the fitted models to make an ensemble prediction. 
#'
#' @details Uses the weighted mean and weighted sample variance to combine
#'   the models. The mean is the weighted mean of all model means and the
#'   variance is the weighted mean of all model variances + the variances of 
#'   the weighted mean using the unbiased estimate of sample variance. See
#'   https://github.com/weecology/portalPredictions/pull/65
#'   We only store the prediction interval for models, so we backcalculate 
#'   individual model variance assuming the same \code{CI_level} throughout. 
#'   Assert that the summed weight of all the model ensembles is 1, as 
#'   that's what the variance estimates assume. The weight values are rounded 
#'   to account for precision errors. Summed weights can also be \code{NA} if
#'   there are not weights availble for that ensemble. 
#' 
#' @param all_forecasts \code{data.frame} of all of the forecasts to be 
#'   combined. 
#' 
#' @param pred_dir \code{character} value of the path to the directory where 
#'   the saved model predictions reside.
#'
#' @param CI_level \code{numeric} confidence interval level to use (must be
#'   between 0 and 1. 
#' 
#' @return ensemble
#' 
#' @export
#'
make_ensemble <- function(all_forecasts, pred_dir, CI_level = 0.9){
  if (!("data.frame" %in% class(all_forecasts))){
    stop("`all_forecasts` is not a data.frame")
  } 
  if (length(pred_dir) > 1){
    stop("`pred_dir` can only be of length = 1")
  }
  if (!is.character(pred_dir)){
    stop("`pred_dir` is not a character")
  }
  if (length(CI_level) > 1){
    stop("`CI_level` can only be of length = 1")
  }
  if (!is.numeric(CI_level)){
    stop("`CI_level` is not numeric")
  }
  if (CI_level <= 0 | CI_level > 1){
    stop("`CI_level` is not between 0 and 1")
  }
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

