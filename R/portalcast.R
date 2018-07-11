#' @title Forecast or hindcast Portal rodents
#'
#' @description Main function for controlling the running of potentially 
#'   multiple models for either a forecast or a hindcast.
#'
#' @param options_all top-level list of options controlling the portalcasting 
#'   directory. Of particular importance is the \code{options_cast} list,
#'   which contains the options controlling the (fore- or hind-)casting 
#'   \cr \cr To run a default hindcast, use 
#'   \code{options_all = all_options(cast_type = "hindcasts")}
#'
#' @return Nothing
#'
#' @export
#'
portalcast <- function(options_all = all_options()){
  clear_tmp(options_all$options_dir$tree)
  verify_models(options_all$options_cast)
  prep_data(options_all$options_data)
  casts(options_all)
}

#' @title Verify that models to forecast or hindcast with exist
#'
#' @description Verify that models requested are available
#'
#' @param options_cast casting options
#'
#' @return nothing
#'
#' @export
#'
verify_models <- function(options_cast = cast_options()){

  if (!options_cast$quiet){
    cat("Checking model availability", "\n")
  }
  model_dir <- sub_path(options_cast$tree, "models")
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist")
  }
  available <- list.files(model_dir)
  if (options_cast$model[1] != "all"){
    models <- options_cast$model
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      stop(paste0("Requested model(s) ", missmod, " not in directory \n"))
    }
  }
  message("All requested models available")
}

#' @title Source the selected models to forecast or hindcast with
#'
#' @description Source the code of the selected model scripts
#'
#' @param options_cast casting options
#'
#' @return nothing
#'
#' @export
#'
cast_models <- function(options_cast = cast_options()){
  if (!options_cast$quiet){
    if (options_cast$cast_type == "forecasts"){
      message("Running models")
    }
    if (options_cast$cast_type == "hindcasts"){ 
      end_step <- options_cast$end[options_cast$hind_step]
      message("#####################################################")
      message(paste0("Running models for initial newmoon ", end_step))
    }
  }
  sapply(models_to_cast(options_cast), source)
}

#' @title Select models to forecast or hindcast with
#'
#' @description Verify that models requested are available and select them
#'
#' @param options_cast casting options
#'
#' @return names of files to be run
#'
#' @export
#'
models_to_cast <- function(options_cast = cast_options()){
  model_dir <- sub_path(options_cast$tree, "models")
  if (options_cast$model[1] == "all"){
    runnames <- list.files(model_dir, full.names = TRUE)
  } else{
    modelnames <- paste0(options_cast$model, ".R")
    torun <- (modelnames %in% list.files(model_dir))
    runnames <- list.files(model_dir, full.names = TRUE)[torun] 
  }
  return(runnames)
}

#' @title Create tmp
#'
#' @description Create the tmp subdirectory specifically
#'
#' @param tree directory tree
#'
#' @return nothing
#'
#' @export
#'
create_tmp <- function(tree = dirtree()){
  opts <- dir_options(base = tree$base, main = tree$main, subs = "tmp")
  create_sub_dirs(opts)
}

#' @title Clear tmp
#'
#' @description Remove all of the files from the tmp subdirectory specifically
#'
#' @param tree directory tree
#'
#' @return nothing
#'
#' @export
#'
clear_tmp <- function(tree = dirtree()){
  temp_dir <- sub_path(tree, "tmp")
  if (!dir.exists(temp_dir)){
    create_tmp(tree)
  }
  if (length(list.files(temp_dir)) > 0){
    file.remove(file_path(tree, paste0("tmp/", list.files(temp_dir))))
  }
}

#' @title Prepare data subdirectory for a forecast run or hindcast runs
#'
#' @description Prepare the data directory for a forecasting run or a set
#'   of hindcasting runs
#'
#' @param options_data the data options list
#'
#' @return nothing
#'
#' @export
#'
prep_data <- function(options_data = data_options()){
  if (!options_data$quiet){
    cat("Preparing data", "\n")
  }
  metadata_path <- file_path(options_data$tree, "data/metadata.yaml")
  if (!file.exists(metadata_path)){
    fill_data(options_data)
  }
  metadata <- yaml.load_file(metadata_path)    
  if (metadata$forecast_date != today()){
    fill_data(options_data)
  }
}

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
  step_casts(options_all)
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
  cast_models(options_cast)
  combined <- combine_forecasts(options_cast)
  ensemble <- add_ensemble(options_cast)

  message("#####################################################")
  clear_tmp(options_cast$tree)
}

#' @title Step through the subsequent casts
#' 
#' @description If there are multiple steps in the hindcast, update the data
#'   and continue casting
#'
#' @param options_all full options list
#'
#' @return nothing
#'
#' @export
#'
step_casts <- function(options_all = all_options()){
  n_steps <- length(options_all$options_data$covariates$end)
  if (n_steps > 1){
    for (i in 2:n_steps){
      options_all <- step_hind_forward(options_all)
      update_data(options_all$options_data)
      cast(options_all$options_cast)
    }  
  }
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
  if (out){
    if (!options_cast$quiet){
      end_step <- options_cast$end[options_cast$hind_step]
      message(paste0("Initial newmoon ", end_step, " not fully sampled"))
      message("#####################################################")
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
  options_all$options_cast$hind_step <- new_step
  return(options_all)
}

