#' @title Forecast or hindcast Portal rodents models
#'
#' @description Run potentially multiple models for either a forecast or a 
#'   hindcast. 
#'
#' @param options_all \code{all_options}-class \code{list} of options 
#'   controlling the portalcasting directory. See \code{\link{all_options}}.
#'   Of particular importance is the \code{options_cast} \code{list},
#'   (see \code{\link{cast_options}}) which contains the options controlling 
#'   the (fore- or hind-)casting.
#'   \cr \cr To run a default hindcast, use 
#'   \code{options_all = all_options(cast_type = "hindcasts")}
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' portalcast()
#' }
#'
#' @export
#'
portalcast <- function(options_all = all_options()){
  check_argsX()
  clear_tmp(options_all$options_dir$tree)
  verify_models(options_all$options_cast)
  prep_data(options_all$options_data)
  casts(options_all)
}

#' @title Verify that models requested to forecast or hindcast with exist
#'
#' @description Verify that models requested have scripts in the models 
#'   subdirectory. 
#'
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @export
#'
verify_models <- function(options_cast = cast_options()){
  check_argsX()
  if (!options_cast$quiet){
    cat("Checking model availability", "\n")
  }
  model_dir <- sub_paths(options_cast$tree, "models")
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

#' @title Source the scripts to run the selected forecast or hindcast models
#'
#' @description Source the R scripts of the selected model(s) indicated by the
#'   \code{model} element in \code{options_cast}.
#'
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @export
#'
cast_models <- function(options_cast = cast_options()){
  check_argsX()
  if (!options_cast$quiet){
    if (options_cast$cast_type == "forecasts"){
      message("##########################################################")
      message("Running models")
    }
    if (options_cast$cast_type == "hindcasts"){ 
      end_step <- options_cast$end[options_cast$hind_step]
      message("##########################################################")
      message(paste0("Running models for initial newmoon ", end_step))
    }
  }
  sapply(models_to_cast(options_cast), source)
}

#' @title Generate the file path(s) to the script(s) of the model(s) forecast
#'   or hindcast with
#'
#' @description Translate a \code{models} vector of model name(s) into a 
#'   \code{character} vector of file path(s) corresponding to the model 
#'   scripts. 
#'
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @return \code{character} vector of the path(s) of the R script file(s) to 
#'   be run.
#'
#' @export
#'
models_to_cast <- function(options_cast = cast_options()){
  check_argsX()
  model_dir <- sub_paths(options_cast$tree, "models")
  if (options_cast$model[1] == "all"){
    runnames <- list.files(model_dir, full.names = TRUE)
  } else{
    modelnames <- paste0(options_cast$model, ".R")
    torun <- (list.files(model_dir) %in% modelnames)
    runnames <- list.files(model_dir, full.names = TRUE)[torun] 
  }
  runnames
}

#' @title Create tmp subdirectory
#'
#' @description Create the tmp subdirectory specifically.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @export
#'
create_tmp <- function(tree = dirtree()){
  check_argsX()
  subs <- subdirs("tmp")
  opts <- dir_options(base = tree$base, main = tree$main, subs = subs)
  create_sub_dirs(opts)
}

#' @title Clear tmp subdirectory
#'
#' @description Remove all of the files from the tmp subdirectory 
#'   specifically.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @export
#'
clear_tmp <- function(tree = dirtree()){
  check_argsX()
  temp_dir <- sub_paths(tree, "tmp")
  if (!dir.exists(temp_dir)){
    create_tmp(tree)
  }
  if (length(list.files(temp_dir)) > 0){
    file.remove(file_paths(tree, paste0("tmp/", list.files(temp_dir))))
  }
}

#' @title Prepare data subdirectory for a forecast or hindcast run(s)
#'
#' @description Make sure that the data subdirectory has the updated files
#'   for a forecasting run or a set of hindcasting runs. Uses the 
#'   \code{metadata.yaml} file to verify validity.
#'
#' @param options_data Class-\code{data_options} \code{list} containing the
#'   options for filling the data subdirectory. See 
#'   \code{\link{data_options}}. 
#'
#' @export
#'
prep_data <- function(options_data = data_options()){
  check_argsX()
  if (!options_data$quiet){
    cat("Preparing data", "\n")
  }
  metadata_path <- file_paths(options_data$tree, "data/metadata.yaml")
  if (!file.exists(metadata_path)){
    fill_data(options_data)
  }
  metadata <- yaml.load_file(metadata_path)    
  if (metadata$forecast_date != today()){
    fill_data(options_data)
  }
  if (options_data$cast_type == "hindcasts"){
    fill_data(options_data)
  }
}

#' @title Run one or potentially multiple forecasts or hindcasts
#' 
#' @description Given that the data and models are prepared accordingly, run 
#'   a set of one or more forecast or hindcasts, (optionally) add an ensemble,
#'   combine the results, and add them to the predictions folder. \cr \cr
#'   \code{casts}: run one or more -casts flexibly
#'
#' @param options_all \code{all_options}-class \code{list} of options 
#'   controlling the portalcasting directory. See \code{\link{all_options}}.
#'
#' @export
#'
casts <- function(options_all = all_options()){
  check_argsX()
  cast(options_all$options_cast)
  step_casts(options_all)
  if (!options_all$options_cast$quiet){
    cat("##########################################################", "\n")
    cat("Models done", "\n")
    cat("##########################################################", "\n")
  }
}

#' @rdname casts
#' 
#' @description \code{cast}: run a single -cast
#'
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @export
#'
cast <- function(options_cast = cast_options()){
  check_argsX()
  if (check_to_skip(options_cast)){
    return()
  }
  cast_models(options_cast)
  combined <- combine_forecasts(options_cast)
  ensemble <- add_ensemble(options_cast)
  if (!options_cast$quiet){
    message("##########################################################")
  }
  clear_tmp(options_cast$tree)
}

#' @rdname casts
#' 
#' @description \code{step_casts}: iterate through subsequent-to-the-first
#'   -cast(s) within \code{casts}. 
#'
#' @export
#'
step_casts <- function(options_all = all_options()){
  check_argsX()
  n_steps <- length(options_all$options_data$covariates$end)
  if (n_steps > 1){
    for (i in 2:n_steps){
      options_all <- step_hind_forward(options_all)
      update_data(options_all$options_data)
      cast(options_all$options_cast)
    }  
  }
}

#' @title Step the hind_step values forward
#'
#' @description Iterate the \code{hind_step} elements of options \code{list}s
#'   through subsequent-to-the-first -cast(s) within \code{\link{casts}}.
#'
#' @param options_all \code{all_options}-class \code{list} of options 
#'   controlling the portalcasting directory. See \code{\link{all_options}}.
#'   
#' @return Updated \code{options_all} \code{list} for the next -cast.
#'
#' @export
#'
step_hind_forward <- function(options_all = all_options()){
  check_argsX()
  new_step <- options_all$options_data$covariates$hind_step + 1
  options_all$options_data$rodents$hind_step <- new_step
  options_all$options_data$covariates$hind_step <- new_step
  options_all$options_cast$hind_step <- new_step
  options_all
}

#' @title Check if the newmoon should be skipped during a set of hindcasts
#' 
#' @description For hindcasts, skip newmoons with incomplete surveys.
#'
#' @param options_cast Class-\code{cast_options} \code{list} containing the
#'   hind- or forecasting options. See \code{\link{cast_options}}. 
#'
#' @return logical indicating if the cast should be skipped
#'
#' @export
#'
check_to_skip <- function(options_cast){
  check_argsX()
  out <- FALSE
  if (options_cast$cast_type == "hindcasts"){ 
    end_step <- options_cast$end[options_cast$hind_step]
    trap_path <- file_paths(options_cast$tree, "data/trapping.csv")
    trap_tab <- read.csv(trap_path, stringsAsFactors = FALSE)
    moon_path <- file_paths(options_cast$tree, "data/moons.csv")
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
      message("##########################################################")
    }
  }
  out
}

#' @title Update the data for each subsequent step in a hindcast
#'
#' @description Rather than re-create the full rodent and covariate files
#'   again, simply step back in time through the hindcast's end vector 
#'   and trim the last "historical" sample off.
#'
#' @param options_data Class-\code{data_options} \code{list} containing the
#'   options for filling the data subdirectory. See 
#'   \code{\link{data_options}}. 
#'
#' @export
#'
update_data <- function(options_data){
  check_argsX()
  if (!options_data$moons$quiet){
    message("Updating forecasting data files in data subdirectory")
  }
  options_data$moons$quiet <- TRUE
  options_data$metadata$quiet <- TRUE
  moons <- prep_moons(options_data$moons)
  rodents_list <- update_rodents_list(options_data$rodents)
  covariates <- update_covariates(moons, options_data$covariates)
  met <- prep_metadata(moons, rodents_list, covariates, options_data$metadata)
}

#' @title Update the covariate data for a step in a hindcast
#'
#' @description Given an updated \code{options_covariates$hind_step}, update
#'   the covariate data.
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param options_covariates A class-\code{covariates_options} \code{list} of 
#'   settings controlling the covariates data creation.
#'
#' @return An updated \code{covariates}-class \code{data.frame} with needed 
#'   forecasted covariates associated with the focal forecast or hindcast 
#'   newmoon.
#'
#' @export
#'
update_covariates <- function(moons, options_covariates){
  check_argsX()
  end_step <- options_covariates$end[options_covariates$hind_step]
  covs_path <- file_paths(options_covariates$tree, "data/covariates.csv")
  covs <- read.csv(covs_path, stringsAsFactors = FALSE) 
  covs <- classy(covs, c("data.frame", "covariates"))
  hist_cov <- covs[covs$newmoonnumber <= end_step, ]
  fcast_cov <- prep_fcast_covariates(hist_cov, moons, options_covariates)
  out <- bind_rows(hist_cov, fcast_cov)
  dataout(out, options_covariates)
}

#' @title Update the rodent data for a step in a hindcast
#'
#' @description Given an updated \code{options_rodents$hind_step}, update
#'   the rodent data.
#'
#' @param options_rodents A class-\code{rodents_options} \code{list} of 
#'   settings controlling the rodents data creation.
#'
#' @return A \code{rodents_list}-class \code{list} of two \code{rodents}-class
#'   \code{data.frames}: \code{"all"} (containing counts across all plots)
#'   \code{"controls"} (containing counts across just the control plots), 
#'   updated for the particular step in the hindcast.
#'
#' @export
#'
update_rodents_list <- function(options_rodents){
  check_argsX()
  end_step <- options_rodents$end[options_rodents$hind_step]
  all_path <- file_paths(options_rodents$tree, "data/all.csv")
  all <- read.csv(all_path, stringsAsFactors = FALSE)
  all <- all[all$newmoonnumber <= end_step, ]
  write.csv(all, all_path, row.names = FALSE)
  all <- classy(all, c("data.frame", "rodents"))

  ctls_path <- file_paths(options_rodents$tree, "data/controls.csv")
  ctls <- read.csv(ctls_path, stringsAsFactors = FALSE)
  ctls <- ctls[ctls$newmoonnumber <= end_step, ]
  write.csv(ctls, ctls_path, row.names = FALSE)
  ctls <- classy(ctls, c("data.frame", "rodents"))
  
  classy(list("all" = all, "controls" = ctls), c("rodents_list", "list"))
}
