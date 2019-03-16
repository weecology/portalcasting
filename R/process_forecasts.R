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
#' @param model \code{character} value of the name of the model.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @export
#'
save_forecast_output <- function(all, controls, model, tree){
  check_args()

  metadata <- read_data(tree, "metadata");
  temp_dir <- sub_paths(tree, "tmp")
  forecasts <- rbind(all$forecast, controls$forecast)
  aics <- rbind(all$aic, controls$aic)

  fcast_fname <- paste0(model, metadata$filename_suffix, ".csv")
  fcast_path <- file.path(temp_dir, fcast_fname)
  write.csv(forecasts, fcast_path, row.names = FALSE)

  aic_fname <- paste0(model, metadata$filename_suffix, "_model_aic.csv")
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
  check_args()
  msg <- paste0("Compiling ", options_cast$cast_type)
  messageq(msg, options_cast$quiet)
  temp_dir <- sub_paths(options_cast$tree, "tmp")
  pred_dir <- sub_paths(options_cast$tree, "predictions")
  forecast_date <- options_cast$cast_date
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
#' @return Forecast abundance table for the ensemble model.
#' 
#' @export
#'
add_ensemble <- function(options_cast = cast_options()){
  check_args()
  if (options_cast$ensemble){
    messageq("Creating ensemble model", options_cast$quiet)
    temp_dir <- sub_paths(options_cast$tree, "tmp")
    pred_dir <- sub_paths(options_cast$tree, "predictions")
    forecast_date <- options_cast$cast_date
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
  check_args()
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
#'   individual model variance assuming the same \code{confidence_level} 
#'   throughout. Assert that the summed weight of all the model ensembles is 
#'   1, as that's what the variance estimates assume. The weight values are 
#'   rounded to account for precision errors. Summed weights can also be 
#'   \code{NA} if there are not weights availble for that ensemble. 
#' 
#' @param all_forecasts \code{data.frame} of all of the forecasts to be 
#'   combined. 
#' 
#' @param pred_dir \code{character} value of the path to the directory where 
#'   the saved model predictions reside.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#' 
#' @return Forecast abundance table for the ensemble model.
#' 
#' @export
#'
make_ensemble <- function(all_forecasts, pred_dir, confidence_level = 0.9){
  check_args()
  if (length(unique(all_forecasts$model)) == 1){
    ensemble <- all_forecasts
    ensemble$model <- "Ensemble"
    ensemble$LowerPI <- ifelse(ensemble$LowerPI < 0, 0, ensemble$LowerPI)
    return(ensemble)
  }
  weights <- compile_aic_weights(pred_dir)
  weights$date <- as.Date(weights$date)
  CI_quantile <- qnorm((1 - confidence_level) / 2, lower.tail = FALSE)

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



#' @title Read in a cast file or multiple cast files and set the class
#'
#' @description Read in a specified forecast or hindcast data file and ensure 
#'   its class attribute is appropriate for usage within the portalcasting 
#'   pipeline. Current not reliably coded for hindcasts.
#'   \cr \cr \code{read_cast}: Only allows for a single date and defaults to
#'   the most recent.
#'   \cr \cr \code{read_casts}: Allows for multiple dates and defaults to all.
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'  
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'   file in the predictions subdirectory. Can only be length 1 and if 
#'   \code{NULL} (default), selects the most recent -casts.
#'  
#' @return Class \code{casts} \code{data.frame} of requested fore- or 
#'   hindcasts.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' portalcast()
#' read_cast()
#' read_casts()
#' }
#'
#' @export
#'
read_cast <- function(tree = dirtree(), cast_type = "forecasts", 
                       cast_date = NULL){
  check_args()
  if (is.null(cast_date)){
    cast_date <- most_recent_cast(tree, cast_type)
  }
  lpath <- paste0("predictions/", cast_date, cast_type, ".csv")
  fpath <- file_paths(tree, lpath)
  if (!file.exists(fpath)){
    stop(paste0(cast_type, " from ", cast_date, " not available"))
  }
  read.csv(fpath, stringsAsFactors = FALSE) %>%
  na_conformer() %>%
  verify_cast() %>%
  classy(c("data.frame", "casts"))
}

#' @rdname read_cast 
#'
#' @param cast_dates \code{Date}s the predictions were made. Used to select 
#'   the files in the predictions subdirectory. Can be length 1 or more and if 
#'   \code{NULL} (default), selects all available -casts.
#'
#' @export
#'
read_casts <- function(tree = dirtree(), cast_type = "forecasts", 
                       cast_dates = NULL){
  check_args()
  if (is.null(cast_dates)){
    pfolderpath <- sub_paths(tree = tree, "predictions")
    pfiles <- list.files(pfolderpath)
    of_interest1 <- grepl(cast_type, pfiles)
    of_interest2 <- grepl("aic", pfiles)
    cast_text <- paste0(cast_type, ".csv")
    cast_dates <- gsub(cast_text, "", pfiles[of_interest1 & !of_interest2])
    cast_dates <- as.Date(cast_dates)
  }
  ndates <- length(cast_dates)
  all_casts <- data.frame()
  for(i in 1:ndates){  
    all_casts <- read_cast(tree, cast_type, cast_dates[i]) %>%
                 bind_rows(all_casts, .)
  }
  classy(all_casts, c("data.frame", "casts"))

}

#' @title Verify that a read-in cast file is formatted appropriately
#'
#' @description Ensure that a -cast file that has been read in is formatted
#'   according to \href{https://bit.ly/2H2z3Jb}{specifications.}
#'
#' @param cast \code{data.frame} -cast file read in.
#'
#' @param cast_to_check \code{data.frame} -cast file being checked within 
#'   \code{cast_is_valid} using \code{\link{check_arg}}.
#'
#' @param verbose \code{logical} indicator if details of failure should
#'   be printed
#'
#' @return \code{verify_cast}: \code{cast} as read in (as long as it is 
#'   valid). \cr \cr
#'   \code{cast_is_valid}: \code{logical} of if the -cast is formatted 
#'   properly.
#' 
#' @export
#'
verify_cast <- function(cast){
  check_args()
  cast
}

#' @rdname verify_cast
#'
#' @export
#'
cast_is_valid <- function(cast_to_check, verbose = FALSE){
  check_args()
  is_valid <- TRUE
  violations <- c()
  valid_columns <- c("date", "forecastmonth", "forecastyear", "newmoonnumber",
                     "model", "currency", "level", "species", "estimate",
                     "LowerPI", "UpperPI", "fit_start_newmoon",
                     "fit_end_newmoon", "initial_newmoon")
  valid_currencies <- c("abundance", "richness", "biomass", "energy")
  valid_levels <- c("All", "Controls", "FullExclosure", "KratExclosure",
                     paste("Plot", 1:24, " ", sep = ""))
  valid_species <- rodent_spp("wtotal")

  if(!(all(colnames(cast) %in% valid_columns) & 
       all(valid_columns %in% colnames(cast)))){
    if(verbose){
      print("file column names invalid")
    }
    return(FALSE)
  }

  cast$date <- as.Date(cast$date, "%Y-%m-%d")
  if(any(is.na(cast$date))){
    is_valid <- FALSE
    violations <- c("date", violations) 
  }
  if(!all(unique(cast$currency) %in% valid_currencies)){
    is_valid <- FALSE
    violations <- c("currency", violations) 
  }
  if(!all(unique(cast$level) %in% valid_levels)){ 
    is_valid <- FALSE
    violations <- c("level", violations) 
  }
  if(!all(unique(cast$species) %in% valid_species)){ 
    is_valid <- FALSE
    violations <- c("species", violations) 
  }
  if(any(is.na(cast$estimate))) { 
    is_valid <- FALSE
    violations <- c("NA esimates", violations) 
  }
  if(any(is.na(cast$LowerPI))) { 
    is_valid <- FALSE
    violations <- c("NA LowerPI", violations) 
  }
  if(any(is.na(cast$UpperPI))) { 
    is_valid <- FALSE
    violations <- c("NA UpperPI", violations) 
  }

  if(!is.integer(cast$fit_start_newmoon)) {
    is_valid <- FALSE
    violations <- c("fit_start_newmoon not int", violations)
  }
  if(!is.integer(cast$fit_end_newmoon)) { 
    is_valid <- FALSE
    violations <- c("fit_end_newmoon not int", violations)
  }
  if(!is.integer(cast$initial_newmoon)) {
    is_valid <- FALSE
    violations <- c("initial_newmoon not int", violations)
  }
  if(any(is.na(cast$fit_start_newmoon))) {
    is_valid <- FALSE
    violations <- c("fit_start_newmoon contains NA", violations)
  }
  if(any(is.na(cast$fit_end_newmoon))) {
    is_valid <- FALSE
    violations <- c("fit_end_newmoon contains NA", violations)
  }
  if(any(is.na(cast$initial_newmoon))) {
    is_valid <- FALSE
    violations <- c("initial_newmoon contains NA", violations)
  }
  
  if(verbose & length(violations) > 0){
    violationses <- paste(violations, collapse = ", ")
    print(paste0("Forecast validation failed: ", violationses))
  }
  is_valid
}

#' @title Determine the most recent forecast or hindcast
#'
#' @description Determine the date of the most recently produced forecast or 
#'   hindcast in a predictions folder.
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'  
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. 
#'
#' @param with_census \code{logical} toggle if the plot should include the
#'   observed data collected during the predicted census.
#'
#' @return \code{Date} of the most recent cast.
#'
#' @examples
#' \dontrun{
#' 
#' setup_dir(all_options(download_existing_predictions = TRUE))
#' most_recent_cast()
#' }
#'
#' @export
#'
most_recent_cast <- function(tree = dirtree(), cast_type = "forecasts",
                             with_census = FALSE){
  check_args()
  pfolderpath <- sub_paths(tree = tree, "predictions")
  pfiles <- list.files(pfolderpath)
  of_interest1 <- grepl(cast_type, pfiles)
  of_interest2 <- grepl("aic", pfiles)
  if (sum(of_interest1 & !of_interest2) < 1){
    stop("no valid files in the predictions folder for `cast_type`")
  }
  cast_text <- paste0(cast_type, ".csv")
  cast_dates <- gsub(cast_text, "", pfiles[of_interest1 & !of_interest2])
  cast_dates <- as.Date(cast_dates)
  prior_to <- today() + 1
  if (with_census){
    prior_to <- most_recent_census(tree)
  }
  max(cast_dates[cast_dates < prior_to])
}

#' @title Select the specific fore- or hindcast from a casts table
#'
#' @description Given a \code{casts}-class \code{data.frame} of many models'
#'   predictions, select a specific subset for use.
#'
#' @param casts Class \code{casts} \code{data.frame} of requested fore- or 
#'   hindcasts to be selected from.
#'
#' @param species \code{character} value(s) of the species code(s) or
#'   \code{"total"} for the total across species. If \code{NULL}, all species 
#'   and "total" are returned. 
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'   or \code{"Controls"}). If \code{NULL}, all levels are returned.
#'
#' @param models \code{character} value(s) of the name(s) (or 
#'   \code{"Ensemble"}) of the model(s) of interest. If \code{NULL}, all
#'   models are returned.
#'
#' @param newmoonnumbers \code{integer}-conformable value(s) of the 
#'   newmoonnumber(s) of interest. If \code{NULL}, all newmoons are returned.
#'
#' @return Class \code{casts} \code{data.frame} of trimmed fore- or 
#'   hindcasts.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' portalcast()
#' casts <- read_casts()
#' scasts <- select_casts(casts)
#' }
#'
#' @export
#'
select_casts <- function(casts, species = NULL, level = NULL, models = NULL, 
                         newmoonnumbers = NULL){
  check_args()
  incl_species <- rep(TRUE, nrow(casts))
  incl_level <- rep(TRUE, nrow(casts))
  incl_model <- rep(TRUE, nrow(casts))
  incl_nmm <- rep(TRUE, nrow(casts))

  casts <- na_conformer(casts)
  if (!is.null(species)){
    incl_species <- casts[ , "species"] %in% species
  }
  if (!is.null(level)){
    incl_level <- casts[ , "level"] %in% level
  }
  if (!is.null(models)){
    incl_model <- casts[ , "model"] %in% models
  }
  if (!is.null(newmoonnumbers)){
    incl_nmm <- casts[ , "newmoonnumber"] %in% newmoonnumbers
  }
  incl <- which(incl_species & incl_level & incl_model & incl_nmm)
  casts[incl, ]
}


#' @title Append the observed values to a table of -casts
#'
#' @description Add a column of observed values and optionally raw error,
#'   in-prediction-window logical indicator, and lead time columns. Additional
#'   columns are added by default.
#'
#' @param casts Class \code{casts} \code{data.frame} of requested fore- or 
#'   hindcasts to have observations added to.
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'
#' @param add_error \code{logical} indicator if the \code{error} column should
#'   be added to the output as well. 
#'
#' @param add_in_window \code{logical} indicator if the \code{in_window} 
#'   column should be added to the output as well. 
#'
#' @param add_lead \code{logical} indicator if the \code{lead} column should
#'   be added to the output as well. 
#'
#' @return Class \code{casts} \code{data.frame} with additional columns.
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir(all_options(download_existing_predictions = TRUE))
#' casts <- read_casts()
#' casts_a <- select_casts(casts, level = "All")
#' casts_a <- append_observed_to_cast(casts_a) 
#' }
#'
#' @export
#'
append_observed_to_cast <- function(casts, tree = dirtree(), add_error = TRUE,
                                    add_in_window = TRUE, add_lead = TRUE){
  check_args()
  level <- unique(casts$level)
  if (length(level) != 1){
    stop("`casts` must have (only) one type for `level` column")
  }
  obs <- read_data(tree, tolower(level)) 
  colnames(obs)[which(colnames(obs) == "NA.")] <- "NA"
  casts$observed <- NA
  for(i in 1:nrow(casts)){
    nmmatch <- which(obs$newmoonnumber == casts$newmoonnumber[i])
    sppmatch <- which(colnames(obs) == casts$species[i])
    obsval <- obs[nmmatch, sppmatch]
    if (length(obsval) == 1){
      casts$observed[i] <- obsval
    }
  }
  if (add_error){
    casts$error <- casts$estimate - casts$observed
  }
  if (add_in_window){
    above_lower <- casts$observed >= casts$LowerPI
    below_upper <- casts$observed <= casts$UpperPI
    casts$in_window <- above_lower & below_upper
  }
  if (add_lead){
    casts$lead <- casts$newmoonnumber - casts$initial_newmoon
  }
  casts
}

#' @title Summarize a table of -casts to -cast-level errors
#'
#' @description Summarize error across all of the data points within each 
#'   forecast or hindcast in a set of -casts. 
#'
#' @param casts Class \code{casts} \code{data.frame} of requested fore- or 
#'   hindcasts with observed values and errors included.
#'
#' @param min_observed \code{integer} value for the minimum number of observed
#'   values needed for a -cast to be retained in the output table. Default is
#'   \code{1}, which returns all -casts with any observations. To include all
#'   -casts (even those without any evaluations), set to \code{0}. 
#'
#' @return \code{data.frame} of errors summarized to the -cast-level. 
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir(all_options(download_existing_predictions = TRUE))
#' casts <- read_casts()
#' casts_a <- select_casts(casts, level = "All")
#' casts_a <- append_observed_to_cast(casts_a)
#' measure_cast_error(casts_a)
#' }
#'
#' @export
#'
measure_cast_error <- function(casts, min_observed = 1){
  check_args()
  groupcols <- c("model", "species", "level", "date", "initial_newmoon")
  cols <- which(colnames(casts) %in% groupcols)
  castgroup <- apply(casts[ ,cols], 1, paste, collapse = "_")
  ugroups <- unique(castgroup)
  ngroups <- length(ugroups)
  
  RMSE <- rep(NA, ngroups)
  coverage <- rep(NA, ngroups)
  nsamples <- rep(NA, ngroups)
  nsampleso <- rep(NA, ngroups)
  tmodel <- rep(NA, ngroups) 
  tspecies <- rep(NA, ngroups)
  tlevel <- rep(NA, ngroups)
  tdate <- rep(NA, ngroups)
  tdate <- rep(NA, ngroups)
  fit_start_newmoon <- rep(NA, ngroups)
  fit_end_newmoon <- rep(NA, ngroups)
  for(i in 1:ngroups){
    incl <- which(castgroup == ugroups[i])
    nsamples[i] <- length(incl)
    nsampleso[i] <- length(na.omit(casts$observed[incl]))
    splitname <- strsplit(ugroups[i], "_")[[1]]
    tmodel[i] <- splitname[2]
    tspecies[i] <- splitname[4] 
    tlevel[i] <- splitname[3] 
    tdate[i] <- splitname[1] 
    errs <- na.omit(casts$error[incl])
    RMSE[i] <- sqrt(mean(errs^2))
    in_window <- na.omit(casts$in_window[incl])
    coverage[i] <- sum(in_window)/length(in_window)
    fit_start_newmoon[i] <- unique(casts$fit_start_newmoon[incl])
    fit_end_newmoon[i] <- unique(casts$fit_end_newmoon[incl])
  }
  casttab <- data.frame(model = tmodel, species = tspecies, level = tlevel, 
                        date = tdate, fit_start_newmoon = fit_start_newmoon,
                        fit_end_newmoon = fit_end_newmoon, 
                        nsamples = nsamples, nsamples_obs = nsampleso, 
                        RMSE = RMSE, coverage = coverage)
  no_obs <- which(nsampleso < min_observed)
  if (length(no_obs) > 0){
    casttab <- casttab[-no_obs, ]    
  }
  casttab
}

#' @title Select the most abundant species from a forecast or hindcast
#'
#' @description Given a forecast or hindcast, determine the most abundant
#'   species predicted. Currently only reliable for forecasts.
#'
#' @param topx \code{integer}-conformable numeric value for the top number of
#'   species to select.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param species \code{character} vector of the species codes (or 
#'   \code{"total"} for the total across species) to be selected from or 
#'   \code{NULL} to include all species and the total.
#'
#' @param level \code{character} value of the level of interest (\code{"All"} 
#'   or \code{"Controls"}).
#'
#' @param cast_type \code{character} value of the type of -cast of model. Used
#'   to select the file in the predictions subdirectory. Currently only 
#'   reliably coded for \code{"forecasts"}.
#'
#' @param cast_date \code{Date} the predictions were made. Used to select the
#'   file in the predictions subdirectory. If \code{NULL} (default), the
#'   most recently made -cast is selected. 
#'
#' @param model \code{character} value of the name (or \code{"Ensemble"}) of
#'   the model to be plotted.
#'
#' @param lead \code{integer}-conformable lead of the newmoon number used to
#'   select the data plotted. 
#'
#' @param from_date \code{Date} to be used as a reference of when to count
#'   the \code{lead} from. If \code{NULL} (default), for 
#'   \code{cast_type = "forecasts"}, \code{from_date = cast_date} and 
#'   \code{plot_cast_point} is not yet reliable for 
#'   \code{cast_type = "hindcasts"}.
#'
#' @return \code{character} vector of length \code{topx} of the species codes
#'   (see \code{\link{rodent_spp}}) of the selected species.
#'
#' @export
#'
select_most_ab_spp <- function(topx = 3, tree = dirtree(), 
                               species = rodent_spp(),
                               level = "Controls", cast_type = "forecasts", 
                               cast_date = NULL, model = "Ensemble", 
                               lead = 1, from_date = NULL){
  check_args()
  if (is.null(cast_date)){
    cast_date <- most_recent_cast(tree, cast_type)
  }
  if (is.null(from_date)){
    if(cast_type == "forecasts"){
      from_date <- cast_date
    }
  }
  metadata <- read_data(tree, "metadata")
  obs <- read_data(tree, tolower(level))
  moons <- read_data(tree, "moons")
  nmdates <- as.Date(as.character(moons$newmoondate))
  most_recent_nm_spot <- max(which(nmdates <= from_date))
  most_recent_nm_number <- moons$newmoonnumber[most_recent_nm_spot]
  cast_nms_io <- metadata$rodent_forecast_newmoons > most_recent_nm_number
  to_include <- which(cast_nms_io)[lead]
  newmoonnumbers <- metadata$rodent_forecast_newmoons[to_include]

  pred <- read_cast(tree, cast_type = cast_type, cast_date = cast_date) %>%
          select_casts(species = species, level = level, models = model,
                      newmoonnumbers = newmoonnumbers)  
  pred$species[order(pred$estimate, decreasing = TRUE)[1:topx]]
}

