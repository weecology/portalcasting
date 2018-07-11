#' @title Forecast or hindcast Portal rodents
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
  clear_tmp(options_all$options_dir$tree)
  verify_models(options_all$options_cast)
#this will be getting looped or whatever
  prep_data(options_all$options_data)
  cast(options_all$options_cast)
  clear_tmp(options_all$options_dir$tree)
#
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