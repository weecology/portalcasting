#' @title Evaluate Forecasts
#'
#' @description Evaluate forecasts in the directory, based on id(s). \cr \cr
#'              Current metrics include raw error (which can be used to calculate root mean squared error; RMSE), coverage, log score, and continuous rank probability score (CRPS). \cr
#'              `read_forecasts_evaluations` read in the forecasts evaluations file. 
#'
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param forecast_id,forecasts_ids `integer` (or integer `numeric`) value(s) representing the forecasts of interest for evaluating, as indexed within the `forecasts` subdirectory. See the forecasts metadata file (`forecasts_metadata.csv`) for summary information. \cr
#'        `forecast_id` can only be a single value, whereas `forecasts_ids` can be multiple.
#'
#' @return A `data.frame` of all forecast evaluations at the observation (newmoon) level, as requested, [`invisible`][base::invisible]-ly.
#'
#' @name evaluate forecasts
#'
#' @aliases evaluate evaluation
#'
#' @family core
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "evaluations")
#'    setup_dir(main = main1)
#'
#'    plot_covariates(main = main1)
#'
#'    make_model_combinations(main = main1)
#'
#'    portalcast(main   = main1, 
#'               models = "AutoArima")
#'
#'    cast(main    = main1,
#'         model   = "AutoArima", 
#'         dataset = "controls", 
#'         species = "DM")
#' 
#'    ## evaluate_forecasts(main = main1) ## extensive runtime for full evaluation from scratch
#'
#'    ids <- select_forecasts(main = main1)$forecast_id
#'         
#'    evaluate_forecast(main        = main1, 
#'                      forecast_id = ids[1])
#'
#'    read_forecasts_evaluations(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
evaluate_forecasts <- function (main         = ".", 
                                forecasts_ids = NULL) {

  settings <- read_directory_settings(main = main)

  forecasts_to_evaluate <- select_forecasts(main          = main, 
                                            forecasts_ids = forecasts_ids)

  forecasts_ids <- forecasts_to_evaluate$forecast_id
  nforecast_ids <- length(forecasts_ids)

  if (NROW(forecasts_to_evaluate) == 0) {

    stop("no forecasts available for request")

  }

  existing_evaluations      <- read_forecasts_evaluations(main = main)
  forecasts_metadata        <- read_forecasts_metadata(main = main)
  rodents_table             <- read_rodents_dataset(main = main, dataset = "all")                          
  last_census_newmoonnumber <- max(rodents_table$newmoonnumber[rodents_table$newmoonnumber %in% rodents_table$newmoonnumber[!is.na(rodents_table[ , "total"])]])

  if (!is.null(existing_evaluations)) {

    all_forecasts_ids           <- forecasts_metadata$forecast_id
    past_forecasts_ids          <- forecasts_metadata$forecast_id[forecasts_metadata$forecast_start_newmoonnumber <= last_census_newmoonnumber]
    complete_evaluation_ids     <- unique(existing_evaluations$forecast_id[existing_evaluations$complete])

    forecasts_left_to_evaluate  <- past_forecasts_ids[!(past_forecasts_ids %in% complete_evaluation_ids)]

 
  } else {

    forecasts_left_to_evaluate  <- forecasts_ids

  }

  selected_forecasts_ids <- forecasts_ids[forecasts_ids %in% forecasts_left_to_evaluate]

  if (length(selected_forecasts_ids) == 0) {

    message(" No forecasts need evaluation.", quiet = settings$quiet)
    return(existing_evaluations)

  }

  nselected_forecasts_ids <- length(selected_forecasts_ids)

  out <- named_null_list(element_names = selected_forecasts_ids)

  messageq("Evaluating forecasts ...\n", quiet = settings$quiet)

  for (i in 1:nselected_forecasts_ids) {

    messageq(paste0("  -", selected_forecasts_ids[i]), quiet = !settings$verbose)

    out[[i]] <- tryCatch(evaluate_forecast(main        = main,
                                           forecast_id = selected_forecasts_ids[i]),
                         error = function(x) {NA})

  }

  nrows_out <- sapply(out, NROW)
  row_1     <- cumsum(c(1, nrows_out[1:(nselected_forecasts_ids - 1)]))
  row_2     <- cumsum(nrows_out) 

  out_flat  <- data.frame(matrix(NA, nrow = row_2[length(row_2)], ncol = ncol(out[[1]])) )
  colnames(out_flat) <- colnames(out[[1]])

  for (i in 1:nselected_forecasts_ids) { 

    out_flat[row_1[i]:row_2[i], ] <- out[[i]]

  }

  out_flat <- rbind(existing_evaluations, out_flat)

  messageq("... done.\n", quiet = settings$quiet)

  write_data(x            = out_flat,
             main         = main,
             subdirectory = settings$subdirectories$forecasts,
             save         = settings$save,
             overwrite    = settings$overwrite, 
             filename     = settings$files$forecasts_evaluations,
             quiet        = settings$quiet)

}

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
evaluate_forecast <- function (main        = ".", 
                               forecast_id = NULL) {

  settings <- read_directory_settings(main = main)

  return_if_null(x = forecast_id)

  forecast_table <- read_forecast_table(main           = main, 
                            forecast_id        = forecast_id)
  forecast_meta <- read_forecast_metadata(main     = main, 
                                  forecast_id  = forecast_id)
  forecast_table <- add_observations_to_forecast_table(main     = main,  
                                  forecast_table = forecast_table)
  model_forecast  <- read_model_forecast(main         = main, 
                                 forecast_id      = forecast_id)

  forecast_table$covered  <- forecast_table$observation >= forecast_table$lower_pi & forecast_table$observation <= forecast_table$upper_pi 
  forecast_table$error    <- forecast_table$estimate - forecast_table$observation 
  forecast_table$logs     <- NA
  forecast_table$crps     <- NA
  forecast_table$complete <- FALSE

  scoring_family <- switch(forecast_meta$model,
                           "AutoArima"                            = "normal",
                           "NaiveArima"                           = "normal",
                           "ESSS"                                 = "normal",
                           "GPEDM"                                = "normal",
                           "simplexEDM"                           = "normal",
                           "nbGARCH"                              = "nbinom",
                           "nbsGARCH"                             = "nbinom",
                           "pGARCH"                               = "poisson",
                           "psGARCH"                              = "poisson",
                           "pevGARCH"                             = "poisson",
                           "jags_RW"                              = "sample",
                           "jags_logistic"                        = "sample",
                           "jags_logistic_competition"            = "sample",
                           "jags_logistic_covariates"             = "sample",
                           "jags_logistic_competition_covariates" = "sample")

  rodents_table             <- read_rodents_dataset(main = main, dataset = "all")                          
  last_census_newmoonnumber <- max(rodents_table$newmoonnumber[rodents_table$newmoonnumber %in% rodents_table$newmoonnumber[!is.na(rodents_table[ , "total"])]])

  can_score <- !is.na(forecast_table$obs)

  if (!any(can_score)) {

    if (all(forecast_table$newmoonnumber <= last_census_newmoonnumber)) {

      forecast_table$complete <- TRUE

    }

    cols_in <- c("forecast_id", "newmoonnumber", "estimate", "lower_pi", "upper_pi", "observation", "covered", "error", "logs", "crps", "complete")
    return(forecast_table[ , cols_in])

  } else {

    forecast_obs  <- forecast_table$observation[can_score]

  }

  if (scoring_family == "normal") {

    forecast_mean <- forecast_table$estimate[can_score]
    forecast_sd   <- pmax(1e-5, (forecast_table$upper_pi[can_score] - forecast_table$estimate[can_score]) / 1.96, na.rm = TRUE)


    forecast_table$logs[can_score] <- logs(y      = forecast_obs,
                                           family = scoring_family,
                                           mean   = forecast_mean,
                                           sd     = forecast_sd)
    forecast_table$crps[can_score] <- crps(y      = forecast_obs,
                                           family = scoring_family,
                                           mean   = forecast_mean,
                                           sd     = forecast_sd)

  } else if (scoring_family == "poisson") {

    forecast_lambda  <- pmax(1e-5, forecast_table$estimate[can_score])

    forecast_table$logs[can_score] <- logs(y      = forecast_obs,
                                           family = scoring_family,
                                           lambda = forecast_lambda)
    forecast_table$crps[can_score] <- crps(y      = forecast_obs,
                                           family = scoring_family,
                                           lambda = forecast_lambda)

  } else if (scoring_family == "nbinom") {

    # mu:   mean
    # size: dispersion
    # var:  variance
    # sd:   standard deviation
    #  var  = sd ^ 2
    #  var  = mu + mu^2 / size
    #  size = mu^2 / (var - mu)

    forecast_mu   <- pmax(1e-5, forecast_table$estimate[can_score])
    forecast_sd   <- pmax(1e-5, (forecast_table$upper_pi[can_score] - forecast_table$estimate[can_score]) / 1.96, na.rm = TRUE)
    forecast_var  <- (forecast_sd) ^ 2
    forecast_size <- pmax(1e-4, (forecast_mu ^ 2) / (forecast_var - forecast_mu), na.rm = TRUE)

    forecast_table$logs[can_score] <- logs(y      = forecast_obs,
                                           family = scoring_family,
                                           mu     = forecast_mu,
                                           size   = forecast_size)

    forecast_table$crps[can_score] <- crps(y      = forecast_obs,
                                           family = scoring_family,
                                           mu     = forecast_mu,
                                           size   = forecast_size)

  } else if (scoring_family == "sample") {

    if (!is.null(model_forecast$sample)) {

      forecast_sample <- t(model_forecast$sample[ , can_score])

      forecast_table$logs[can_score] <- logs_sample(y   = forecast_obs,
                                                    dat = forecast_sample)

      forecast_table$crps[can_score] <- crps_sample(y   = forecast_obs,
                                                    dat = forecast_sample)

    } else {

      # in the instance that it's a sample but there's no sample, use nbinom

      # mu:   mean
      # size: dispersion
      # var:  variance
      # sd:   standard deviation
      #  var  = sd ^ 2
      #  var  = mu + mu^2 / size
      #  size = mu^2 / (var - mu)

      forecast_mu   <- pmax(1e-5, forecast_table$estimate[can_score])
      forecast_sd   <- pmax(1e-5, (forecast_table$upper_pi[can_score] - forecast_table$estimate[can_score]) / 1.96, na.rm = TRUE)
      forecast_var  <- (forecast_sd) ^ 2
      forecast_size <- pmax(1e-4, (forecast_mu ^ 2) / (forecast_var - forecast_mu), na.rm = TRUE)

      forecast_table$logs[can_score] <- logs(y      = forecast_obs,
                                             family = "nbinom",
                                             mu     = forecast_mu,
                                             size   = forecast_size)

      forecast_table$crps[can_score] <- crps(y      = forecast_obs,
                                             family = "nbinom",
                                             mu     = forecast_mu,
                                             size   = forecast_size)

    }

  }

  species                    <- ifelse(forecast_table$species[1] == "NA", "NA", forecast_table$species[1])
  rodents_table              <- read_rodents_dataset(main = main, dataset = forecast_table$dataset[1])                          
  last_census_newmoonnumber  <- max(rodents_table$newmoonnumber[rodents_table$newmoonnumber %in% rodents_table$newmoonnumber[!is.na(rodents_table[ , species])]])
  forecast_table$complete    <- forecast_table$forecast_end_newmoonnumber <= last_census_newmoonnumber
  cols_in                    <- c("forecast_id", "newmoonnumber", "estimate", "lower_pi", "upper_pi", "observation", "covered", "error", "logs", "crps", "complete")
  forecast_table[ , cols_in]

}

#'
#' @rdname evaluate-forecasts
#'
#' @export
#'
read_forecasts_evaluations <- function (main = "."){
  
  settings  <- read_directory_settings(main = main)
  eval_path <- forecasts_evaluations_path(main = main)

  if (!file.exists(eval_path)) {

    messageq("  Forecast evaluations not available.", quiet = settings$quiet)

    out <- NULL

  } else {

    out <- as.data.frame(read_csv_arrow(file = eval_path))

    if ("species" %in% colnames(out)) {
      out <- na_conformer(out)
    } 

  }

  invisible(out)

}



