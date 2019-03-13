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
#' @examples
#' \dontrun{
#'
#' setup_dir()
#' }
#'
#' @export
#'
setup_dir <- function(options_all = all_options()){
  check_args(options_all = options_all)
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
#' @examples
#' \dontrun{
#'
#' create_dir()
#' create_main_dir()
#' create_sub_dirs()
#' create_sub_dir()
#' }
#'
#' @export
#'
create_dir <- function(options_dir = dir_options()){
  check_args(options_dir = options_dir)
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
  check_args(options_dir = options_dir)
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
  check_args(options_dir = options_dir)
  subs <- sub_paths(tree = options_dir$tree)
  sub_dirs <- sapply(subs, create_sub_dir, quiet = options_dir$quiet)
}

#' @rdname create_dir
#'
#' @description \code{create_sub_dir}: Create a specific subdirectory 
#'   folder if it does not already exist.
#'
#' @param path The normalized path of the specific subdirectory folder to be 
#'   created in the directory tree as a \code{character} value (see 
#'   \code{\link{normalizePath}}, \code{\link{sub_path}}).
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'   quieted.
#'
#' @export
#'
create_sub_dir <- function(path = NULL, quiet = FALSE){
  check_args(path = path)
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

#' @title Populate the components of a portalcasting directory
#'
#' @description \code{fill_dir}: Populate the files of an existing 
#'    portalcasting directory structure in full. (Future flexibility will 
#'    allow for selective population of components.)
#'
#' @param options_all Class-\code{all_options} list containing all options 
#'   available for controlling the set up and population of the directory (see 
#'   \code{\link{all_options}}). 
#' 
#' @examples
#' \dontrun{
#'
#' create_dir()
#' fill_dir()
#' fill_PortalData()
#' fill_data()
#' fill_predictions()
#' fill_models()
#' }
#' 
#' @export
#'
fill_dir <- function(options_all = all_options()){
  check_args(options_all = options_all)
  fill_PortalData(options_all$options_PortalData)
  fill_data(options_all$options_data)
  fill_predictions(options_all$options_predictions)
  fill_models(options_all$options_models)
}

#' @rdname fill_dir
#'
#' @description \code{fill_portalData}: Populate the components of the 
#'   PortalData folder.
#'
#' @param options_PortalData Class-\code{PortalData_options} list containing 
#'   available for controlling the set up and population of the PortalData
#'   folder in the portalcasting directory (see 
#'   \code{\link{PortalData_options}}). 
#'
#' @export
#'
fill_PortalData <- function(options_PortalData = PortalData_options()){
  check_args(options_PortalData = options_PortalData)
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

#' @rdname fill_dir
#'
#' @description \code{fill_data}: Populate the for-use data folder in the 
#'   portalcasting directory, including the historical covariates, trapping 
#'   table, moons, rodents, covariates, and metadata files.
#'
#' @param options_data Class-\code{data_options} list containing available
#'   for controlling the set up and population of the data folder in the 
#'   portalcasting directory (see \code{\link{data_options}}). 
#'
#' @export
#'
fill_data <- function(options_data = data_options()){
  check_args(options_data = options_data)
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

#' @rdname fill_dir
#'
#' @description \code{fill_predictions}: Populate the predictions folder with 
#'   existing predictions housed on the main portalPredictions repo site.
#'
#' @param options_predictions Class-\code{predictions_options} list containing 
#'   available for controlling the set up and population of the predictions
#'   folder in the portalcasting directory (see 
#'   \code{\link{predictions_options}}). 
#'
#' @export
#'
fill_predictions <- function(options_predictions = predictions_options()){
  check_args(options_predictions = options_predictions)
  if (options_predictions$download_existing_predictions){
    if (!options_predictions$quiet){
      message("Downloading predictions into predictions subdirectory")
    }
    download_predictions(tree = options_predictions$tree)
  }
}

#' @rdname fill_dir
#'
#' @description \code{fill_models}: Populate the model folder with specified 
#'   model scripts to be run.
#'
#' @param options_models Class-\code{models_options} list containing 
#'   available for controlling the set up and population of the models
#'   folder in the portalcasting directory (see 
#'   \code{\link{models_options}}). 
#'
#' @export
#'
fill_models <- function(options_models = models_options()){
  check_args(options_models = options_models)
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

#' @title Verify that the PortalData subdirectory is present and has required 
#'   data
#'
#' @description Check that the PortalData subdirectory exists and has the
#'   needed specified file(s) within the "Rodents" subdirectory. If any of
#'    the file(s) is(/are) not present, the (entire) directory is filled.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param filename \code{character}-valued vector of name(s) of the file(s) to
#'   use for specific checking. Default (\code{moon_dates.csv}) settings use
#'   the moon dates table.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#' 
#' @examples
#' \dontrun{
#'
#' setup_dir()
#' verify_PortalData()
#' }
#'
#' @export
#' 
verify_PortalData <- function(tree = dirtree(), filename = "moon_dates.csv",
                              quiet = FALSE){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.character(filename)){
    stop("`filename` is not a character")
  }
  path <- file_path(tree = tree, paste0("PortalData/Rodents/", filename)) 
  if (!all(file.exists(path))){
    options_dir <- dir_options()
    options_dir$tree <- tree
    options_dir$quiet <- quiet
    create_dir(options_dir)
    options_PortalData <- PortalData_options()
    options_PortalData$tree <- tree
    options_PortalData$quiet <- quiet
    fill_PortalData(options_PortalData)
  }
}

#' @title Remove the temporary files in PortalData and tmp
#'
#' @description Remove content in specified (by \code{options_dir$to_cleanup})
#'   subdirectories and the subdirectories themselves.
#'
#' @param options_all Class-\code{all_options} list containing all options 
#'   available for controlling the set up and population of the directory (see 
#'   \code{\link{all_options}}). 
#' 
#' @examples
#' \dontrun{
#'
#' setup_dir()
#' portalcast()
#' cleanup_dir()
#' }
#'
#' @export
#' 
cleanup_dir <- function(options_all = all_options()){
  check_args(options_all)
  options_dir <- options_all$options_dir
  subs <- sub_path(options_dir$tree, options_dir$to_cleanup)
  if (!options_dir$quiet){
    subnames <- options_dir$to_cleanup
    if (length(subnames) > 0){
      subnames2 <- paste(subnames, collapse = " ")
      msg <- paste("removing", subnames2, "subdirectories", sep = " ")
    } else {
      msg <- "no subdirectories requested to be removed"
    }
    message(msg)
  }
  cleaned <- sapply(subs, unlink, recursive = TRUE, force = TRUE)
}
