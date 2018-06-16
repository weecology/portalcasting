#' @title Set up a full forecasting directory
#'
#' @description Create and populate a full forecasting directory or any 
#'   missing components.
#'
#' @param base name of the base directory where the forecasting 
#'   directory should exist (will be created if it doesn't)
#'
#' @param main name of the portalcasting directory
#'
#' @param main name of the portalcasting directory
#'
#' @param models names of model scripts to include
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
setup_dir <- function(base = "~", main = "forecasting", subs = subdirs(), 
                      model = models(), quiet = FALSE){
  tree <- dirtree(base = base, main = main, subs = subs)
  create_dir(tree = tree, quiet = quiet)
  fill_dir(tree = tree, model = model, quiet = quiet)
}

#' @title Create the full structure of a portalcasting directory
#'
#' @description Create a full portalcasting directory or any missing 
#'   components.
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
create_dir <- function(tree = dirtree(), quiet = FALSE){
  create_main_dir(tree = tree, quiet = quiet)
  create_sub_dirs(tree = tree, quiet = quiet)
}

#' @title Create the main directory folder of a portalcasting directory
#'
#' @description Create the top-level portalcasting directory folder
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
create_main_dir <- function(tree = dirtree(), quiet = FALSE){
  main <- main_path(tree = tree)
  if (!dir.exists(main)){
    if (!quiet){
      cat(paste0("Creating main directory at ", main, ". \n"))
    }
    dir.create(main)
  }
}

#' @title Create the sub directory folders of a portalcasting directory
#'
#' @description Create a the portalcasting directory subfolders
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
create_sub_dirs <- function(tree = dirtree(), quiet = FALSE){
  subs <- sub_paths(tree = tree)
  sub_dirs <- sapply(subs, create_sub_dir, quiet = quiet)
}

#' @title Create a subdirectory folder 
#'
#' @description Create a specific subdirectory folder if it does not already
#'   exist
#'
#' @param path path of the specific folder to be created
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
create_sub_dir <- function(path = NULL, quiet = FALSE){
  if (is.null(path)){
    return()
  }
  if (!dir.exists(path)){
    if (!quiet){
      cat(paste0("Creating ", basename(path), " sub-directory. \n"))
    }
    dir.create(path)
  }
}

#' @title Populate the base components of a portalcasting directory
#'
#' @description Populate an existing portalcasting directory structure in full
#'    or any missing components.
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
fill_dir <- function(tree = dirtree(), model = models(), quiet = FALSE){
  fill_PortalData(tree = tree, quiet = quiet)
  fill_data(tree = tree, quiet = quiet)
  fill_models(tree = tree, model = model, quiet = quiet)
}

#' @title Populate the raw data of a portalcasting directory
#'
#' @description Populate the raw data folder
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
fill_PortalData <- function(tree = dirtree(), quiet = FALSE){
  if (!quiet){
    cat("Downloading raw data into PortalData subdirectory. \n")
  }
  PD <- download_observations(main_path(tree))
}

#' @title Populate the for-use data of a portalcasting directory
#'
#' @description Populate the for-use data folder
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
fill_data <- function(tree = dirtree(), start = 217, quiet = FALSE){
  if (!quiet){
    cat("Loading forecasting data files into data subdirectory. \n")
  }
  moons <- prep_moons(tree = tree)
  rodents <- prep_rodents(tree = tree, moons = moons, start = start)
  # covariates <- prep_covariates
  # metadata <- prep_metadata
}

#' @title Populate the model scripts of a portalcasting directory
#'
#' @description Populate the model folder with specified scripts to be run
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param models names of model scripts to include
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
fill_models <- function(tree = dirtree(), model = models(), quiet = FALSE){
  mods <- sapply(write_model, model, tree = tree, quiet = qiet)
}









