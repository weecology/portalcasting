#' @title Forecast or hindcast Portal rodents models
#'
#' @description Run potentially multiple models for either a forecast or a 
#'   hindcast. 
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param data_control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{NULL}, which retrieves the model names 
#'  from \code{\link{model_names}}.
#'
#' @param cast_type -cast type: \code{"forecasts"} or \code{"hindcasts"}.
#'  \strong{NOTE:} takes precendence over the value of 
#'  \code{cast_type} in the control lists.
#'
#' @param ensemble \code{logical} indicator if the ensemble should be added.
#'
#' @param end \code{integer} (or integer \code{numeric}) newmoon number of the
#'  last sample to be included. Default value is \code{NULL}, which equates
#'  to the most recently included sample. If \code{cast_type} is 
#'  \code{"hindcasts"} and \code{end} is \code{NULL}, default becomes 
#'  \code{490:403}, corresponding to \code{2017-01-01} back to
#'  \code{2010-01-01}.
#'  \strong{NOTE:} takes precendence over the value of 
#'  \code{end} in the control lists.
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
portalcast <- function(models = NULL, cast_type = "forecasts", end = NULL,
                       tree = dirtree(), quiet = FALSE, ensemble = TRUE, 
                       data_control = list()){
  data_control <- do.call("data_control", data_control)
  if(cast_type == "hindcasts"){
    data_control[["covariates"]]$cast_type <- "hindcasts"
    data_control[["metadata"]]$cast_type <- "hindcasts"
    data_control[["rodents"]]$cast_type <- "hindcasts"
    end <- ifnull(end, 490:403)
    data_control[["rodents"]]$end <- end
    data_control[["covariates"]]$end <- end
    data_control <- do.call("data_control", data_control)
  }
  msg1 <- "##########################################################"
  version_number <- packageDescription("portalcasting", fields = "Version")
  msg2 <- paste0("This is portalcasting v", version_number)
  messageq(c(msg1, msg2), quiet)
  models <- ifnull(models, model_names())
  clear_tmp(tree)
  verify_models(models, tree, quiet)
  prep_data(tree, quiet, data_control)
  casts(models, tree, quiet, data_control, ensemble)
}

#' @title Verify that models requested to forecast or hindcast with exist
#'
#' @description Verify that models requested have scripts in the models 
#'   subdirectory. 
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param models \code{character} vector of the names of models to verify. 
#'
#' @export
#'
verify_models <- function(models = NULL, tree = dirtree(), quiet = FALSE){
  messageq("Checking model availability", quiet)
  model_dir <- sub_paths(tree, "models")
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist")
  }
  available <- list.files(model_dir)
  if (models[1] != "all"){
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      stop(paste0("Requested model(s) ", missmod, " not in directory \n"))
    }
  }
  messageq("All requested models available", quiet)
}

#' @title Source the scripts to run the selected forecast or hindcast models
#'
#' @description Source the R scripts of the selected model(s) indicated by the
#'   \code{model} element in \code{options_cast}.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param data_control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{NULL}, which retrieves the model names 
#'  from \code{\link{model_names}}.
#'
#' @export
#'
cast_models <- function(models = NULL, tree = dirtree(), quiet = FALSE, 
                        data_control = list()){
  data_control <- do.call("data_control", data_control)
  msg1 <- "##########################################################"
  msg2 <- "Running models"
  if (data_control$covariates$cast_type == "hindcasts"){ 
    end_step <- data_control$covariates$end[data_control$covariates$hind_step]
    msg2 <- paste0(msg2, " for initial newmoon ", end_step)  
  }
  messageq(c(msg1, msg2), quiet)
  sapply(models_to_cast(models, tree), source)
}

#' @title Generate the file path(s) to the script(s) of the model(s) forecast
#'   or hindcast with
#'
#' @description Translate a \code{models} vector of model name(s) into a 
#'   \code{character} vector of file path(s) corresponding to the model 
#'   scripts. 
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{NULL}, which retrieves the model names 
#'  from \code{\link{model_names}}.
#'
#' @return \code{character} vector of the path(s) of the R script file(s) to 
#'   be run.
#'
#' @export
#'
models_to_cast <- function(models = NULL, tree = dirtree()){
  model_dir <- sub_paths(tree, "models")
  if (models[1] == "all"){
    runnames <- list.files(model_dir, full.names = TRUE)
  } else{
    modelnames <- paste0(models, ".R")
    torun <- (list.files(model_dir) %in% modelnames)
    runnames <- list.files(model_dir, full.names = TRUE)[torun] 
  }
  runnames
}


#' @title Prepare data subdirectory for a forecast or hindcast run(s)
#'
#' @description Make sure that the data subdirectory has the updated files
#'   for a forecasting run or a set of hindcasting runs. Uses the 
#'   \code{metadata.yaml} file to verify validity.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#' 
#' @param control A \code{list} of arguments to control the filling of
#'   the data subdirectory. See \code{\link{data_control}}. 
#'   Arguments not specified assume default values.
#'
#' @export
#'
prep_data <- function(tree = dirtree(), quiet = FALSE, control = list()){
  control <- do.call("data_control", control)
  messageq("Preparing data", quiet)
  metadata_path <- file_paths(tree, "data/metadata.yaml")
  if (!file.exists(metadata_path)){
    fill_data(tree, quiet, control)
  }
  metadata <- yaml.load_file(metadata_path)    
  if (metadata$forecast_date != today()){
    fill_data(tree, quiet, control)
  }
  if (control$covariates$cast_type == "hindcasts"){
    fill_data(tree, quiet, control)
  }
}

#' @title Run one or potentially multiple forecasts or hindcasts
#' 
#' @description Given that the data and models are prepared accordingly, run 
#'   a set of one or more forecast or hindcasts, (optionally) add an ensemble,
#'   combine the results, and add them to the predictions folder. \cr \cr
#'   \code{casts}: run one or more -casts flexibly \cr \cr
#'   \code{cast}: run a single -cast
#'   \code{step_casts}: iterate through subsequent-to-the-first
#'   -cast(s) within \code{casts}. 
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param data_control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'
#' @param models \code{character} vector of the names of models to include in
#'  the pipeline. Defaults to \code{NULL}, which retrieves the model names 
#'  from \code{\link{model_names}}.
#'
#' @param ensemble \code{logical} indicator if the ensemble should be added.
#'
#' @export
#'
casts <- function(models = NULL, tree = dirtree(), quiet = FALSE,
                  data_control = list(), 
                  ensemble = TRUE){
  data_control <- do.call("data_control", data_control)
  cast(models, tree, quiet, data_control, ensemble)
  step_casts(models, tree, quiet, data_control, ensemble)
  msg1 <- "########################################################"
  messageq(c(msg1, "Models done", msg1), quiet)
}

#' @rdname casts
#'
#' @export
#'
cast <- function(models = NULL, tree = dirtree(), quiet = FALSE,
                 data_control = list(), 
                 ensemble = TRUE){
  data_control <- do.call("data_control", data_control)
  if (check_to_skip(tree, quiet, data_control)){
    return()
  }
  cast_models(models, tree, quiet, data_control)
  combined <- combine_forecasts(tree, quiet, data_control)
  ensemble <- add_ensemble(tree, quiet, data_control, ensemble)
  messageq("########################################################", quiet)
  clear_tmp(tree, quiet)
}

#' @rdname casts
#'
#' @export
#'
step_casts <- function(models = NULL, tree = dirtree(), quiet = FALSE,
                       data_control = list(),
                       ensemble = TRUE){
  data_control <- do.call("data_control", data_control)
  n_steps <- length(data_control$covariates$end)
  if (n_steps > 1){
    for (i in 2:n_steps){
      data_control <- step_hind_forward(data_control)
      update_data(tree, quiet, data_control)
      cast(models, tree, quiet, data_control, ensemble)
    }  
  }
}

#' @title Step the hind_step values forward
#'
#' @description Iterate the \code{hind_step} elements of options \code{list}s
#'   through subsequent-to-the-first -cast(s) within \code{\link{casts}}.
#'
#' @param data_control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'   
#' @return Updated \code{options_all} \code{list} for the next -cast.
#'
#' @export
#'
step_hind_forward <- function(data_control = list()){
  data_control <- do.call("data_control", data_control)
  new_step <- data_control$covariates$hind_step + 1
  data_control$rodents$hind_step <- new_step
  data_control$covariates$hind_step <- new_step
  data_control
}

#' @title Check if the newmoon should be skipped during a set of hindcasts
#' 
#' @description For hindcasts, skip newmoons with incomplete surveys.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param data_control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'
#' @return logical indicating if the cast should be skipped
#'
#' @export
#'
check_to_skip <- function(tree = dirtree(), quiet = FALSE, 
                          data_control = list()){
  data_control <- do.call("data_control", data_control)
  out <- FALSE
  if (data_control$covariates$cast_type == "hindcasts"){ 
    end_step <- data_control$covariates$end[data_control$covariates$hind_step]
    trap_path <- file_paths(tree, "data/trapping.csv")
    trap_tab <- read.csv(trap_path, stringsAsFactors = FALSE)
    moon_path <- file_paths(tree, "data/moons.csv")
    moons <- read.csv(moon_path, stringsAsFactors = FALSE)
    min_plots <- data_control$rodents$min_plots
    min_traps <- data_control$rodents$min_traps
    incs <- find_incomplete_censuses(trap_tab, min_plots, min_traps)
    end_step_p <- moons$period[moons$newmoonnumber == end_step]
    if (is.na(end_step_p)){
      out <- TRUE
    } else if (end_step_p %in% incs$period){
      out <- TRUE
    }
  }
  if (out){
    end_step <- data_control$covariates$end[data_control$covariates$hind_step]
    msg1 <- paste0("Initial newmoon ", end_step, " not fully sampled")
    msg2 <- "##########################################################"
    messageq(c(msg1, msg2), quiet)
  }
  out
}

#' @title Update the data for each subsequent step in a hindcast
#'
#' @description Rather than re-create the full rodent and covariate files
#'   again, simply step back in time through the hindcast's end vector 
#'   and trim the last "historical" sample off.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control A \code{list} of arguments to control the filling of
#'  the data subdirectory. See \code{\link{data_control}}. 
#'  Values not set assume defaults.
#'
#' @export
#'
update_data <- function(tree = dirtree, quiet = FALSE, control = list()){
  data_control <- do.call("data_control", control)
  messageq("Updating forecasting data files in data subdirectory", quiet)
  moons <- prep_moons(tree, TRUE, control$moons)
  rodents <- update_rodents_list(tree, control$rodents)
  covar <- update_covariates(moons, tree, quiet, control$covariates)
  md <- prep_metadata(moons, rodents, covar, tree, TRUE, control$metadata)
}

#' @title Update the covariate data for a step in a hindcast
#'
#' @description Given an updated \code{options_covariates$hind_step}, update
#'   the covariate data.
#'
#' @param moons Class-\code{moons} \code{data.frame} containing the historic 
#'   and future newmoons, as produced by \code{\link{prep_moons}}.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param control A \code{list} of arguments to control the coviariates
#'  data. See \code{\link{covariates_control}}. Values not set assume 
#'  defaults.
#'
#' @return An updated \code{covariates}-class \code{data.frame} with needed 
#'   forecasted covariates associated with the focal forecast or hindcast 
#'   newmoon.
#'
#' @export
#'
update_covariates <- function(moons, tree = dirtree(), quiet = FALSE,
                              control = list()){
  control <- do.call("covariates_control", control)
  end_step <- control$end[control$hind_step]
  covs_path <- file_paths(tree, "data/covariates.csv")
  covs <- read.csv(covs_path, stringsAsFactors = FALSE) 
  covs <- classy(covs, c("data.frame", "covariates"))
  hist_cov <- covs[covs$newmoonnumber <= end_step, ]
  fcast_cov <- prep_fcast_covariates(hist_cov, moons, tree, quiet, control)
  out <- bind_rows(hist_cov, fcast_cov)
  dataout(out, tree, control)
}

#' @title Update the rodent data for a step in a hindcast
#'
#' @description Given an updated \code{options_rodents$hind_step}, update
#'   the rodent data.
#'
#' @param tree \code{dirtree}-class directory tree list. Consisting of 
#'  \code{base}, \code{main}, and \code{subs} elements. See
#'  \code{\link{dirtree}}.
#'
#' @param control A \code{list} of arguments to control the rodents
#'  data. See \code{\link{rodents_control}}. Values not set assume defaults.
#'
#' @return A \code{rodents_list}-class \code{list} of two \code{rodents}-class
#'   \code{data.frames}: \code{"all"} (containing counts across all plots)
#'   \code{"controls"} (containing counts across just the control plots), 
#'   updated for the particular step in the hindcast.
#'
#' @export
#'
update_rodents_list <- function(tree = dirtree(), control = list()){
  control <- do.call("rodents_control", control)
  end_step <- control$end[control$hind_step]
  all_path <- file_paths(tree, "data/all.csv")
  all <- read.csv(all_path, stringsAsFactors = FALSE)
  all <- all[all$newmoonnumber <= end_step, ]
  write.csv(all, all_path, row.names = FALSE)
  all <- classy(all, c("data.frame", "rodents"))

  ctls_path <- file_paths(tree, "data/controls.csv")
  ctls <- read.csv(ctls_path, stringsAsFactors = FALSE)
  ctls <- ctls[ctls$newmoonnumber <= end_step, ]
  write.csv(ctls, ctls_path, row.names = FALSE)
  ctls <- classy(ctls, c("data.frame", "rodents"))
  
  classy(list("all" = all, "controls" = ctls), c("rodents_list", "list"))
}