#' @title Add ensemble model to forecasts
#' 
#' @description Add the predictions of an ensemble model to the forecasting 
#'   files.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample.  
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @return Forecast abundance table for the ensemble model.
#' 
#' @export
#'
add_ensemble <- function(main = ".", end_moon = NULL, cast_date = Sys.Date(), 
                         confidence_level = 0.9, quiet = FALSE){

  messageq("Creating ensemble model", quiet)
  temp_dir <- sub_paths(main, "tmp")
  pred_dir <- sub_paths(main, "predictions")
  last_moon <- pass_and_call(last_newmoon)
  end_moon <- ifnull(end_moon, last_moon)
  filename_suffix <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  cclass <- c("Date", "integer", "integer", "integer", "character", 
              "character", "character", "character", "numeric",
              "numeric", "numeric", "integer", "integer", "integer")
  all_casts <- do.call(rbind, 
               lapply(files, read.csv, na.strings = "", colClasses  = cclass))

  ensemble <- pass_and_call(make_ensemble, all_casts = all_casts) 
  ensemble <- select(ensemble, colnames(all_casts))
  cast_date <- as.character(cast_date)
  cast_fname <- paste0(cast_date, filename_suffix, ".csv")
  cast_filename <- file.path(pred_dir, cast_fname)
  append_csv(ensemble, cast_filename)
  ensemble
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
#'   individual model variance assuming the same \code{confidence_level} 
#'   throughout. Assert that the summed weight of all the model ensembles is 
#'   1, as that's what the variance estimates assume. The weight values are 
#'   rounded to account for precision errors. Summed weights can also be 
#'   \code{NA} if there are not weights available for that ensemble. 
#' 
#' @param all_casts \code{data.frame} of all of the forecasts to be 
#'   combined. 
#' 
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#' 
#' @return Forecast abundance table for the ensemble model.
#' 
#' @export
#'
make_ensemble <- function(all_casts, main = ".", confidence_level = 0.9){
  if (length(unique(all_casts$model)) == 1){
    ensemble <- all_casts
    ensemble$model <- "Ensemble"
    ensemble$LowerPI <- ifelse(ensemble$LowerPI < 0, 0, ensemble$LowerPI)
    return(ensemble)
  }
  weights <- compile_aic_weights(main)
  weights$date <- as.Date(weights$date)
  CI_quantile <- qnorm((1 - confidence_level) / 2, lower.tail = FALSE)

  leftj <- c("date", "model", "currency", "level", "species", 
             "fit_start_newmoon", "fit_end_newmoon", "initial_newmoon")
  grp <- quos(date, newmoonnumber, castmonth, castyear, level, 
           currency, species, fit_start_newmoon, fit_end_newmoon, 
           initial_newmoon)
  mod_var <- quo(((UpperPI - estimate)/CI_quantile) ^ 2)
  est <- quo(sum(estimate * weight))
  wtss <- quo(sum(weight * (estimate - ensemble_estimate)^2))
  ens_var <- quo(sum(model_var * weight) + 
                 weighted_ss / (n() * sum(weight) - 1))
  weighted_estimates <- all_casts %>%
                        mutate(model_var = !!mod_var) %>%
                        left_join(weights, by = leftj) %>%
                        group_by(!!!grp) %>%
                        summarize(ensemble_estimate = !!est, 
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
  ensemble
}

#' @title Calculate model weights
#' 
#' @description Calculate AIC-based weights across the models for the full
#'   time series.
#' 
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @return \code{data.frame} of model weights.
#' 
#' @export
#'
compile_aic_weights <- function(main = "."){
  pred_dir <- sub_paths(main, "predictions")

  aic_files <- list.files(pred_dir, full.names = TRUE, recursive = TRUE)
  aic_files <- aic_files[grepl("model_aic", aic_files)]

  aics <- do.call(rbind, 
      lapply(aic_files, read.csv, na.strings = "", stringsAsFactors = FALSE))
 
  grps <- quos(date, currency, level, species, fit_start_newmoon, 
            fit_end_newmoon, initial_newmoon)
 
  aics %>%
  group_by(!!!grps) %>%
  mutate(delta_aic = aic - min(aic), 
         weight = exp(-0.5 * delta_aic) / sum(exp(-0.5*delta_aic))) %>%
  ungroup()
}



#' @title Combine casts and append them to existing files
#' 
#' @description Combine all new casts (from the tmp subdirectory) into
#'  and append the results to the existing files.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @return \code{list} of [1] \code{"casts"} (the casted abundances)
#'  and [2] \code{"all_model_aic"} (the model AIC values).
#'
#' @export
#'
combine_casts <- function(main = ".", moons = prep_moons(main = main),
                          end_moon = NULL, 
                          cast_date = Sys.Date(), quiet = FALSE){

  messageq("Compiling casts", quiet)
  temp_dir <- sub_paths(main, "tmp")
  pred_dir <- sub_paths(main, "predictions")
  last_moon <- pass_and_call(last_newmoon)
  end_moon <- ifnull(end_moon, last_moon)
  filename_suffix <- ifelse(end_moon == last_moon, "forecast", "hindcast")
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  col_class <- c("Date", "integer", "integer", "integer", "character", 
                 "character", "character", "character", "numeric", "numeric",
                 "numeric", "integer", "integer", "integer")
  casts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = col_class))
  file_ptn <- paste(filename_suffix, "_model_aic.csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)

  aics <- do.call(rbind, lapply(files, read.csv, na.strings = ""))
  
  cast_date <- as.character(cast_date)
  cast_fname <- paste(cast_date, filename_suffix, ".csv", sep = "")
  cast_filename <- file.path(pred_dir, cast_fname)
  aic_fname <- paste(cast_date, filename_suffix, "_model_aic.csv", sep = "")
  model_aic_filename <- file.path(pred_dir, aic_fname)
  append_csv(casts, cast_filename)
  append_csv(aics, model_aic_filename)
  
  list(casts = casts, all_model_aic = aics)
}


#' @title Save cast output to files
#'
#' @description Save out the cast abundances and AIC tables from a given 
#'   set of models.
#'
#' @param all Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "all" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param controls Output from a model function (e.g., 
#'   \code{\link{AutoArima}}) run on the "controls" data. Required to be a 
#'   \code{list} with two elements named \code{forecast} and \code{aic}.
#'
#' @param model \code{character} value of the name of the model.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @export
#'
save_cast_output <- function(all, controls, model, main){
  metadata <- read_data(main, "metadata");
  temp_dir <- sub_paths(main, "tmp")
  casts <- rbind(all$cast, controls$cast)
  aics <- rbind(all$aic, controls$aic)

  cast_fname <- paste0(model, metadata$cast_type, ".csv")
  cast_path <- file.path(temp_dir, cast_fname)
  write.csv(casts, cast_path, row.names = FALSE)

  aic_fname <- paste0(model, metadata$cast_type, "_model_aic.csv")
  aic_path <- file.path(temp_dir, aic_fname)
  write.csv(aics, aic_path, row.names = FALSE)

}