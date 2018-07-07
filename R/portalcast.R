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
  temp_dir <- sub_path(tree, "tmp")
  if (!dir.exists(temp_dir)){
    dir.create(temp_dir)
  }

  if (!options_all$options_cast$quiet){
    cat("Preparing data", "\n")
  }
  if (options_all$options_cast$cast_type == "forecasts"){
    metadata_path <- file_path(tree, "data/metadata.yaml")
    if (!file.exists(metadata_path)){
      fill_data(options_all$options_data)
    }
    metadata <- yaml.load_file(metadata_path)    
    if (metadata$forecast_date != today()){
      fill_data(options_all$options_data)
      metadata <- yaml.load_file(metadata_path)
    }
  }

  if (!options_all$options_cast$quiet){
    cat("Checking model availability", "\n")
  }
  model_dir <- sub_path(tree, "models")
  runnames <- select_models(model_dir, options_all$options_cast$model)

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

  file.remove(file_path(tree, paste0("tmp/", list.files(temp_dir))))
}

#' @title Select Models to Forecast or Hindcast With
#'
#' @description Verify that models requested are available and select them
#'
#' @param model_dir directory name where the model files reside
#'
#' @param models names of models to run or "all" to run them all
#'
#' @return names of files to be run
#'
#' @export
#'
select_models <- function(model_dir, models){
  available <- list.files(model_dir)
  if (models[1] == "all"){
    runnames <- list.files(model_dir, full.names = TRUE)
  } else{
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      stop(paste0("Requested model(s) ", missmod, " not in directory \n"))
    }
    runnames <- list.files(model_dir, full.names = TRUE)[torun]
  }
  return(runnames)
}