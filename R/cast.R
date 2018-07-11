#' @title Multiple forecast or hindcast
#' 
#' @description Given the data and models are prepared accordingly, run the
#'   forecast or hindcasts, (optionally) add an ensemble, combine the 
#'   results into the predictions folder.
#'
#' @param options_all full options list
#'
#' @return nothing
#'
#' @export
#'
casts <- function(options_all = all_options()){
  cast(options_all$options_cast)
  n_steps <- length(options_all$options_data$covariates$end)
  if (n_steps > 1){
    for (i in 2:n_steps){
      options_all <- step_hind_forward(options_all)
      update_data(options_all$options_data)
      cast(options_all$options_cast)
    }  
  }
}

#' @title Actually forecast or hindcast
#' 
#' @description Given the data and models are prepared accordingly, run the
#'   forecast or hindcast, (optionally) add an ensemble, combine the 
#'   results into the predictions folder.
#'
#' @param options_cast casting options list
#'
#' @return nothing
#'
#' @export
#'
cast <- function(options_cast = cast_options()){

  if (check_to_skip(options_cast)){
    return()
  }

  if (!options_cast$quiet){
    if (options_cast$cast_type == "forecasts"){
      message("Running models")
    }
    if (options_cast$cast_type == "hindcasts"){ 
      end_step <- options_cast$end[options_cast$hind_step]
      message(paste0("Running models for initial newmoon ", end_step))
    }
  }
  sapply(models_to_cast(options_cast), source)

  if (!options_cast$quiet){
    message(paste0("Compiling ", options_cast$cast_type))
  }
  predictions_dir <- sub_path(options_cast$tree, "predictions")
  combined <- combine_forecasts(options_cast)

  if (options_cast$ensemble){
    if (!options_cast$quiet){
      cat("Creating ensemble model", "\n")
    }
    ensemble <- add_ensemble(options_cast)
  }

  clear_tmp(options_cast$tree)
}


#' @title Check if the newmoon should be skipped
#' 
#' @description For hindcasts, skip casting newmoons with incomplete surveys
#'
#' @param options_cast casting options list
#'
#' @return logical indicating if the cast should be skipped
#'
#' @export
#'
check_to_skip <- function(options_cast){
  out <- FALSE
  if (options_cast$cast_type == "hindcasts"){ 
    end_step <- options_cast$end[options_cast$hind_step]
    trap_path <- file_path(options_cast$tree, "data/trapping.csv")
    trap_tab <- read.csv(trap_path, stringsAsFactors = FALSE)
    moon_path <- file_path(options_cast$tree, "data/moons.csv")
    moons <- read.csv(moon_path, stringsAsFactors = FALSE)
    min_plots <- options_cast$min_plots
    min_traps <- options_cast$min_traps
    incs <- find_incomplete_censuses(trap_tab, min_plots, min_traps)
    end_step_p <- moons$period[moons$newmoonnumber == end_step]
    if (is.na(end_step_p)){
      out <- TRUE
    } else if (end_step_p %in% incs$period){
      out <- TRUE
    }
  }
  out
}

#' @title Update the data for each subsequent step in a hindcast
#'
#' @description Rather than re-create the full rodent and covariate files
#'   again, simply step back in time through the hindcast's end vector 
#'   and trim the last "historical" sample off
#'
#' @param options_data data options list
#'
#' @return nothing
#'
#' @export
#'
update_data <- function(options_data){
  if (!options_data$moons$quiet){
    message("Updating forecasting data files in data subdirectory")
  }
  options_data$moons$quiet <- TRUE
  options_data$metadata$quiet <- TRUE
  moons <- prep_moons(options_data$moons)
  rodents <- update_rodents(options_data$rodents)
  covariates <- update_covariates(moons, options_data$covariates)
  meta <- prep_metadata(moons, rodents, covariates, options_data$metadata)
}

#' @title Update the covariate data for a step in a hindcast
#'
#' @description Given an updated hind_step, update the covariate data 
#'
#' @param moons current newmoon table
#'
#' @param options_covariates covariates options list
#'
#' @return nothing
#'
#' @export
#'
update_covariates <- function(moons, options_covariates){
  end_step <- options_covariates$end[options_covariates$hind_step]
  covs_path <- file_path(options_covariates$tree, "data/covariates.csv")
  covs <- read.csv(covs_path, stringsAsFactors = FALSE)
  hist_cov <- covs[covs$newmoonnumber <= end_step, ]
  fcast_cov <- prep_fcast_covariates(hist_cov, moons, options_covariates)
  out <- bind_rows(hist_cov, fcast_cov)
  dataout(out, options_covariates)
}

#' @title Update the rodent data for a step in a hindcast
#'
#' @description Given an updated hind_step, update the rodent data objects
#'
#' @param options_rodents rodents options list
#'
#' @return a list of two dataframes, all plots and control plots, updated
#'
#' @export
#'
update_rodents <- function(options_rodents){
  end_step <- options_rodents$end[options_rodents$hind_step]
  all_path <- file_path(options_rodents$tree, "data/all.csv")
  all <- read.csv(all_path, stringsAsFactors = FALSE)
  all <- all[all$newmoonnumber <= end_step, ]
  write.csv(all, all_path, row.names = FALSE)

  ctls_path <- file_path(options_rodents$tree, "data/controls.csv")
  ctls <- read.csv(ctls_path, stringsAsFactors = FALSE)
  ctls <- ctls[ctls$newmoonnumber <= end_step, ]
  write.csv(ctls, ctls_path, row.names = FALSE)

  list("all" = all, "controls" = ctls)
}

#' @title Step the hind_step values forward
#'
#' @description Step all of the hind_step values in a full options control 
#'   list forward one
#'
#' @param options_all all options control list
#'
#' @return updated options_all
#'
#' @export
#'
step_hind_forward <- function(options_all = all_options()){
  new_step <- options_all$options_data$covariates$hind_step + 1
  options_all$options_data$rodents$hind_step <- new_step
  options_all$options_data$covariates$hind_step <- new_step
  return(options_all)
}

