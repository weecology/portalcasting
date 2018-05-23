#' @title Combine Forecasts and Append to Existing Files
#' 
#' @description Combine all new forecasts (from the tmp directory), add 
#'   append the results to the existing files
#' 
#' @param model_metadata model metadata list
#' @return list of [1] the forecasts and [2] the model AIC values
#' @export
#'
combine_forecasts <- function(model_metadata){
  
  forecast_date <- model_metadata$forecast_date
  filename_suffix <- model_metadata$filename_suffix
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files("tmp", pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  fcasts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))

  file_ptn <- paste(filename_suffix, "_model_aic.csv", sep = "")
  files <- list.files("tmp", pattern = file_ptn, full.names = TRUE)

  aics <- do.call(rbind, lapply(files, read.csv, na.strings = ""))
  
  fcast_date <- as.character(forecast_date)
  fcast_fname <- paste(fcast_date, filename_suffix, ".csv", sep = "")
  forecast_filename <- file.path("predictions", fcast_fname)
  aic_fname <- paste(fcast_date, filename_suffix, "_model_aic.csv", sep = "")
  model_aic_filename <- file.path("predictions", aic_fname)
  append_csv(fcasts, forecast_filename)
  append_csv(aics, model_aic_filename)
  
  return(list(forecasts = fcasts, all_model_aic = aics))
}

#' @title Add Ensemble Model to Forecasts
#' 
#' @description Add ensembles to the forecast files
#' 
#' @param model_metadata model metadata list
#' @return list of [1] the forecasts and [2] the model AIC values
#' @export
#'
add_ensemble <- function(model_metadata){

  forecast_date <- model_metadata$forecast_date
  filename_suffix <- model_metadata$filename_suffix
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files("tmp", pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  fcasts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))
  fcast_date <- as.character(forecast_date)
  fcast_fname <- paste(fcast_date, filename_suffix, ".csv", sep = "")
  forecast_filename <- file.path("predictions", fcast_fname)

  ensemble <- make_ensemble(fcasts) %>% 
              subset(select = colnames(fcasts))
  append_csv(ensemble, forecast_filename)
}

#' @title Calculate Model Weights
#' @description calculate akaike weights across the models
#' @param forecast_folder folder where the forecast files are
#' @return model weights
#' @export
#'
compile_aic_weights <- function(forecast_folder = "./predictions"){

  date <- NULL
  currency <- NULL
  level <- NULL
  species <- NULL
  fit_start_newmoon <- NULL
  fit_end_newmoon <- NULL
  initial_newmoon <- NULL
  aic <- NULL
  delta_aic <- NULL

  aic_files <- list.files(forecast_folder, full.names = TRUE, 
                          recursive = TRUE)
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


#' Create the ensemble model from all other forecasts
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
#' @param all_forecasts alll forecasts
#' @param models_to_use models to use
#' @param CI_level confidence interval level
#' @return ensemble
#' @export
#'
make_ensemble <- function(all_forecasts, models_to_use = NA, CI_level = 0.9){

  newmoonnumber <- NULL
  forecastmonth <- NULL
  forecastyear <- NULL
  currency <- NULL
  level <- NULL
  species <- NULL
  fit_start_newmoon <- NULL
  fit_end_newmoon <- NULL
  initial_newmoon <- NULL
  UpperPI <- NULL
  estimate <- NULL
  weight <- NULL
  model_var <- NULL
  weighted_ss <- NULL
  n <- NULL
  ensemble_estimate <- NULL
  ensemble_var <- NULL
  LowerPI <- NULL
  sum_weight <- NULL

  weights <- compile_aic_weights()
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

