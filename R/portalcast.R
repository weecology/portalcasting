#' @title Forecast or Hindcast Portal rodents
#'
#' @description Main function for controlling the running of potentially 
#'   multiple models for either a forecast or a hindcast. \cr \cr 
#'   Note: currently \code{portalcast} can only run "forecasts".
#'
#' @param options_all top-level list of options controlling the portalcasting 
#'   directory. Of particular importance is the \code{options_cast} list,
#'   which contains the options controlling the (fore- or hind-)casting
#'
#' @return Nothing
#'
#' @export
#'
portalcast <- function(options_all = all_options()){
  tree <- options_all$options_cast$tree
 
  create_tmp(tree)

  prep_data(options_all$options_data)
  metadata <- yaml.load_file(file_path(tree, "data/metadata.yaml"))


  runnames <- prep_models(options_all$options_models)

  if (!options_all$options_cast$quiet){
    cat("Running models", "\n")
  }
  sapply(runnames, source)

  if (!options_all$options_cast$quiet){
    cat("Compiling forecasts", "\n")
  }
  predictions_dir <- sub_path(tree, "predictions")
  combined <- combine_forecasts(metadata, temp_dir, predictions_dir)

  if (options_all$options_cast$ensemble){
    if (!options_all$options_cast$quiet){
      cat("Creating ensemble model", "\n")
    }
    ensemble <- add_ensemble(metadata, temp_dir, predictions_dir)
  }

  clear_tmp(tree)
}

#' @title Select Models to Forecast or Hindcast With
#'
#' @description Verify that models requested are available and select them
#'
#' @param options_models models options
#'
#' @return names of files to be run
#'
#' @export
#'
prep_models <- function(options_models = models_options()){

  if (!options_models$quiet){
    cat("Checking model availability", "\n")
  }
  model_dir <- sub_path(options_models$tree, "models")

  available <- list.files(model_dir)
  if (options_models$model[1] == "all"){
    runnames <- list.files(model_dir, full.names = TRUE)
  } else{
    modelnames <- paste0(options_models$model, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      stop(paste0("Requested model(s) ", missmod, " not in directory \n"))
    }
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
  file.remove(file_path(tree, paste0("tmp/", list.files(temp_dir))))
}