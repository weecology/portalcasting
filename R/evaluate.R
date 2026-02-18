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

  # Ensure forecast tables are extracted (Zenodo stores them in forecast_id_YYYY-MM-DD.zip)
  forecast_path <- file.path(main, settings$subdirectories$forecasts)
  zip_unzip(type = "unzip", forecast_path = forecast_path)

  existing_evaluations      <- read_forecasts_evaluations(main = main)
  forecasts_metadata        <- read_forecasts_metadata(main = main)
  rodents_table             <- read_rodents_dataset(main = main, dataset = "all")                          
  last_census_newmoonnumber <- max(rodents_table$newmoonnumber[rodents_table$newmoonnumber %in% rodents_table$newmoonnumber[!is.na(rodents_table[ , "total"])]])

  past_forecasts_ids <- forecasts_metadata$forecast_id[forecasts_metadata$forecast_start_newmoonnumber <= last_census_newmoonnumber]

  if (!is.null(existing_evaluations)) {

    if ("complete" %in% colnames(existing_evaluations)) {
      complete_evaluation_ids   <- unique(existing_evaluations$forecast_id[existing_evaluations$complete %in% c(TRUE, 1)])
    } else {
      complete_evaluation_ids   <- integer(0)
      messageq("  Evaluations file has no 'complete' column; treating all as needing evaluation.", quiet = settings$quiet)
    }
    forecasts_left_to_evaluate  <- past_forecasts_ids[!(past_forecasts_ids %in% complete_evaluation_ids)]

  } else {

    forecasts_left_to_evaluate  <- past_forecasts_ids

  }

  # When user specifies forecasts_ids, restrict to those that need evaluation.
  # When NULL, evaluate all forecasts_left_to_evaluate (not limited by select_forecasts QAQC filter).
  if (!is.null(forecasts_ids)) {
    forecasts_to_evaluate <- select_forecasts(main = main, forecasts_ids = forecasts_ids)
    if (NROW(forecasts_to_evaluate) == 0) stop("no forecasts available for request")
    requested_ids        <- forecasts_to_evaluate$forecast_id
    selected_forecasts_ids <- requested_ids[requested_ids %in% forecasts_left_to_evaluate]
  } else {
    selected_forecasts_ids <- forecasts_left_to_evaluate
  }

  if (length(selected_forecasts_ids) == 0) {

    message(" No forecasts need evaluation.", quiet = settings$quiet)
    return(existing_evaluations)

  }

  # Require forecast_table files to exist; stop if any are missing for analysis
  forecast_path <- file.path(main, settings$subdirectories$forecasts)
  expected_file <- function(id) file.path(forecast_path, paste0("forecast_id_", id, "_forecast_table.csv"))
  has_table <- vapply(selected_forecasts_ids, function(id) file.exists(expected_file(id)), logical(1))
  n_skipped <- sum(!has_table)
  if (n_skipped > 0) {
    missing_ids   <- selected_forecasts_ids[!has_table]
    first_missing <- missing_ids[1]
    tbl_full      <- normalizePath(expected_file(first_missing), mustWork = FALSE)
    stop("Forecast table file does not exist for forecast_id ", first_missing, ". Path: ", tbl_full,
         " (", n_skipped, " forecast(s) total missing) -- analyze why the file(s) are missing.")
  }

  if (length(selected_forecasts_ids) == 0) {

    message(" No forecasts need evaluation (none had forecast_table files).", quiet = settings$quiet)
    return(existing_evaluations)

  }

  nselected_forecasts_ids <- length(selected_forecasts_ids)

  eval_csv     <- file.path(forecast_path, settings$files$forecasts_evaluations)
  eval_parquet <- file.path(forecast_path, "forecasts_evaluations.parquet")
  incremental  <- settings$save

  # Ensure we have a CSV file to append to when saving (parquet cannot be appended easily)
  if (incremental && !file.exists(eval_csv) && !is.null(existing_evaluations) && NROW(existing_evaluations) > 0) {
    if (file.exists(eval_parquet)) unlink(eval_parquet)
    # Drop rows that are all NA before writing
    not_empty <- !apply(is.na(existing_evaluations), 1, all)
    write_csv_arrow(x = existing_evaluations[not_empty, , drop = FALSE], file = eval_csv)
    messageq("  Initialized evaluations CSV from existing data.", quiet = settings$quiet)
  }

  messageq(if (incremental) "Evaluating forecasts (writing each to file) ...\n" else "Evaluating forecasts ...\n", quiet = settings$quiet)

  out_list <- if (!incremental) vector("list", nselected_forecasts_ids) else NULL

  for (i in 1:nselected_forecasts_ids) {

    fid      <- selected_forecasts_ids[i]
    tbl_path <- expected_file(fid)
    tbl_full <- normalizePath(tbl_path, mustWork = FALSE)

    messageq(paste0("  -", fid), quiet = !settings$verbose)

    if (!file.exists(tbl_path)) {
      stop("Forecast table file does not exist at forecast_id ", fid, ". Path: ", tbl_full, " -- analyze why the file is missing.")
    }

    new_rows <- evaluate_forecast(main = main, forecast_id = fid)

    # Drop rows that are all NA (empty rows) - do not write them
    if (NROW(new_rows) > 0) {
      not_empty <- !apply(is.na(new_rows), 1, all)
      new_rows  <- new_rows[not_empty, , drop = FALSE]
    }

    if (incremental && NROW(new_rows) > 0) {
      write_header <- !file.exists(eval_csv)
      utils::write.table(new_rows,
                        file      = eval_csv,
                        append    = !write_header,
                        sep       = ",",
                        col.names = write_header,
                        row.names = FALSE,
                        qmethod   = "double")
      rm(new_rows)
      gc(verbose = FALSE)
    } else if (!incremental) {
      out_list[[i]] <- new_rows
    }
  }

  if (incremental) {
    # Convert CSV to parquet (archive format); read prefers parquet over CSV
    if (file.exists(eval_csv)) {
      eval_data <- as.data.frame(read_csv_arrow(file = eval_csv))
      # Drop rows that are all NA before writing parquet
      if (NROW(eval_data) > 0) {
        not_empty <- !apply(is.na(eval_data), 1, all)
        eval_data <- eval_data[not_empty, , drop = FALSE]
      }
      arrow::write_parquet(eval_data, sink = eval_parquet)
      rm(eval_data)
      gc(verbose = FALSE)
    }
    messageq("... done.\n", quiet = settings$quiet)
    return(invisible(read_forecasts_evaluations(main = main)))
  }

  # save=FALSE: flatten in memory and return (no file write)
  nrows_out   <- sapply(out_list, NROW)
  row_1       <- cumsum(c(1, nrows_out[1:(nselected_forecasts_ids - 1)]))
  row_2       <- cumsum(nrows_out)
  last_row    <- if (length(row_2) > 0) row_2[length(row_2)] else 0L
  num_cols    <- ncol(out_list[[1]])
  out_flat    <- data.frame(matrix(NA, nrow = last_row, ncol = num_cols))
  colnames(out_flat) <- colnames(out_list[[1]])
  for (i in seq_len(nselected_forecasts_ids)) {
    out_flat[row_1[i]:row_2[i], ] <- out_list[[i]]
  }
  out_flat <- rbind(existing_evaluations, out_flat)
  # Drop rows that are all NA (empty rows)
  if (NROW(out_flat) > 0) {
    not_empty <- !apply(is.na(out_flat), 1, all)
    out_flat  <- out_flat[not_empty, , drop = FALSE]
  }
  messageq("... done.\n", quiet = settings$quiet)
  invisible(out_flat)
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
                           "sNaiveArima"                          = "normal",
                           "sAutoArima"                           = "normal",
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
  
  settings     <- read_directory_settings(main = main)
  forecast_path <- file.path(main, settings$subdirectories$forecasts)
  eval_zip     <- file.path(forecast_path, "forecasts_evaluations.zip")
  eval_csv     <- file.path(forecast_path, "forecasts_evaluations.csv")
  eval_parquet <- file.path(forecast_path, "forecasts_evaluations.parquet")

  # Unzip evaluations if they are stored in a zip file (e.g. from Zenodo archive)
  if (file.exists(eval_zip)) {
    utils::unzip(eval_zip, exdir = forecast_path, junkpaths = TRUE)
    unlink(eval_zip)
  }

  # Read from parquet (preferred, smaller) or csv, whichever exists
  eval_path <- NULL
  if (file.exists(eval_parquet)) {
    eval_path <- eval_parquet
  } else if (file.exists(eval_csv)) {
    eval_path <- eval_csv
  }

  if (is.null(eval_path)) {

    messageq("  Forecast evaluations not available.", quiet = settings$quiet)
    out <- NULL

  } else if (tools::file_ext(eval_path) == "parquet") {

    out <- as.data.frame(arrow::read_parquet(eval_path))
    if ("species" %in% colnames(out)) {
      out <- na_conformer(out)
    }

  } else {

    out <- as.data.frame(read_csv_arrow(file = eval_path))
    if ("species" %in% colnames(out)) {
      out <- na_conformer(out)
    }

  }

  # Drop rows that are all NA (empty rows)
  if (!is.null(out) && NROW(out) > 0) {
    n_before  <- NROW(out)
    not_empty <- !apply(is.na(out), 1, all)
    out       <- out[not_empty, , drop = FALSE]
    n_dropped <- n_before - NROW(out)
    if (n_dropped > 0) {
      messageq("  Dropped ", n_dropped, " empty row(s) (all NA) from forecasts evaluations.", quiet = settings$quiet)
    }
  }

  invisible(out)

}



