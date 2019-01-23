#' @title Set up a full forecasting directory
#'
#' @description Create (via \code{\link{create_dir}}) and populate (via 
#'   \code{\link{fill_dir}}) a full portalcasting directory. Presently, this
#'   function overwrites any existing components of the directory. Future
#'   iterations will allow for updating, overwriting, or neither.
#'
#' @param options_all Class-\code{all_options} list containing all options 
#'   available for controlling the set up and population of the directory (see 
#'   \code{\link{all_options}}). 
#' 
#' @export
#'
setup_dir <- function(options_all = all_options()){
  if (!("all_options" %in% class(options_all))){
    stop("`options_all` is not an all_options list")
  }
  create_dir(options_all$options_dir)
  fill_dir(options_all)
}

#' @title Create the structure of a portalcasting directory
#'
#' @description \code{create_dir}: Create a full portalcasting directory or 
#'   any missing components.
#'
#' @param options_dir Class-\code{dir_options} list containing all options 
#'   available for controlling the set up of the directory (see 
#'   \code{\link{dir_options}}). 
#'
#' @export
#'
create_dir <- function(options_dir = dir_options()){
  if (!("dir_options" %in% class(options_dir))){
    stop("`options_dir` is not a dir_options list")
  }
  create_main_dir(options_dir)
  create_sub_dirs(options_dir)
}

#' @rdname create_dir
#'
#' @description \code{create_main_dir}: Create the main level of the 
#'   portalcasting directory.
#'
#' @export
#'
create_main_dir <- function(options_dir = dir_options()){
  if (!("dir_options" %in% class(options_dir))){
    stop("`options_dir` is not a dir_options list")
  }
  main <- main_path(tree = options_dir$tree)
  if (!dir.exists(main)){
    if (!options_dir$quiet){
      message(paste0("Creating main directory at ", main))
    }
    dir.create(main)
  }
}

#' @rdname create_dir
#'
#' @description \code{create_sub_dirs}: Create the sub level folders of the 
#'   portalcasting directory.
#'
#' @export
#'
create_sub_dirs <- function(options_dir = dir_options()){
  subs <- sub_paths(tree = options_dir$tree)
  sub_dirs <- sapply(subs, create_sub_dir, quiet = options_dir$quiet)
}

#' @rdname create_dir
#'
#' @description \code{create_sub_dir}: Create a specific subdirectory 
#'   folder if it does not already exist.
#'
#' @param path The normalized path of the specific subdirectory folder to be 
#'   created in the directory tree as a character value (see 
#'   \code{\link{normalizePath}}, \code{\link{sub_path}}).
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'   quieted.
#'
#' @export
#'
create_sub_dir <- function(path = NULL, quiet = FALSE){
  if (is.null(path)){
    return()
  }
  if (!dir.exists(path)){
    if (!quiet){
      message(paste0("Creating ", basename(path), " subdirectory"))
    }
    dir.create(path)
  }
}

#' @title Populate the base components of a portalcasting directory
#'
#' @description Populate an existing portalcasting directory structure in full
#'    or any missing components.
#'
#' @param options_all list containing all options available for controlling
#'   the set up and population of the forecast directory 
#'
#' @return Nothing
#'
#' @export
#'
fill_dir <- function(options_all = all_options()){
  fill_PortalData(options_all$options_PortalData)
  fill_data(options_all$options_data)
  fill_predictions(options_all$options_predictions)
  fill_models(options_all$options_models)
}

#' @title Populate the raw data of a portalcasting directory
#'
#' @description Populate the raw data folder
#'
#' @param options_PortalData PortalData subdirectory options
#'
#' @return Nothing
#'
#' @export
#'
fill_PortalData <- function(options_PortalData = PortalData_options()){
  if (!options_PortalData$quiet){
    message("Downloading raw data into PortalData subdirectory")
  }
  base_folder <- main_path(options_PortalData$tree)
  version <- options_PortalData$version
  from_zenodo <- options_PortalData$from_zenodo
  if (options_PortalData$quiet){
    suppressMessages(
      PD <- download_observations(base_folder, version, from_zenodo)
    )
  } else{
    PD <- download_observations(base_folder, version, from_zenodo)
  }
}

#' @title Populate the for-use data of a portalcasting directory
#'
#' @description Populate the for-use data folder
#'
#' @param options_data list containing all options available for controlling
#'   the set up and population of the forecast data subdirectory  
#'
#' @return Nothing
#'
#' @export
#'
fill_data <- function(options_data = data_options()){
  if (!options_data$moons$quiet){
    message("Loading forecasting data files into data subdirectory")
  }
  transfer_hist_covariate_forecasts(options_data)
  transfer_trapping_table(options_data)
  moons <- prep_moons(options_data$moons)
  rodents <- prep_rodents(moons, options_data$rodents)
  covariates <- prep_covariates(moons, options_data$covariates)
  meta <- prep_metadata(moons, rodents, covariates, options_data$metadata)
}

#' @title Populate the predictions subdirectory of a portalcasting directory
#'
#' @description Populate the predictions folder with existing predictions
#'   housed on the main portalPredictions repo
#'
#' @param options_predictions predictions options list
#'
#' @return Nothing
#'
#' @export
#'
fill_predictions <- function(options_predictions = predictions_options()){
  if (options_predictions$download_existing_predictions){
    if (!options_predictions$quiet){
      message("Downloading predictions into predictions subdirectory")
    }
    download_predictions(tree = options_predictions$tree)
  }
}

#' @title Populate the model scripts of a portalcasting directory
#'
#' @description Populate the model folder with specified scripts to be run
#'
#' @param options_models model option list
#'
#' @return Nothing
#'
#' @export
#'
fill_models <- function(options_models = models_options()){
  if (!options_models$quiet){
    message("Adding prefab models to models subdirectory:")
  }
  nmods <- length(options_models$model)
  for (i in 1:nmods){
    modname <- options_models$model[i]
    funname <- paste0(modname, "_options")
    options_model <- do.call(funname, list("tree" = options_models$tree))
    options_model$quiet <- options_models$quiet
    mod <- write_model(options_model)
  }
}

#' @title Verify that the PortalData sub is present and has required data
#'
#' @description Check that the PortalData subdirectory exists and has the
#'   needed file. If the file is not present, the directory is filled.
#'
#' @param tree directory tree
#'
#' @param filename name of the file to specifically check
#'
#' @return nothing
#'
#' @export
#' 
verify_PortalData <- function(tree = dirtree(), filename = "moon_dates.csv"){
  path <- file_path(tree = tree, paste0("PortalData/Rodents/", filename)) 
  if (!file.exists(path)){
    options_dir <- dir_options()
    options_dir$tree <- tree
    create_dir(options_dir)
    fill_PortalData(options_dir)
  }
}

#' @title Remove the temporary files in PortalData and tmp
#'
#' @description Remove the PortalData and tmp subdirectories.
#'
#' @param options_all list containing all options available for controlling
#'   the set up and population of the forecast directory (see 
#'   \code{\link{all_options}})
#'
#' @return nothing
#'
#' @export
#' 
cleanup_dir <- function(options_all = all_options()){
  tree <- options_all$options_dir$tree
  PD_dir <- sub_path(tree, "PortalData")
  temp_dir <- sub_path(tree, "tmp")
  unlink(PD_dir, recursive = TRUE, force = TRUE)
  unlink(temp_dir, recursive = TRUE, force = TRUE)
}
