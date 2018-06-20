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
#' @param subs names of the portalcasting subdirectories
#'
#' @param model names of model scripts to include
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
#' @param model names of model scripts to include
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
#' @param all_options list of lists of data options
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
fill_data <- function(tree = dirtree(), all_options = all_data_options(), 
                      quiet = FALSE){
  if (!quiet){
    cat("Loading forecasting data files into data subdirectory. \n")
  }
  moons <- prep_moons(tree, all_options$moons_options)
  rodents <- prep_rodents(tree, moons, all_options$rodents_options)

# working on prep_covariates in that script
  # covariates <- prep_covariates(tree, moons, all_options$covariates_options)
  # metadata <- prep_metadata
}

#' @title Populate the model scripts of a portalcasting directory
#'
#' @description Populate the model folder with specified scripts to be run
#'
#' @param tree the name tree of the portalcasting directory
#'
#' @param model names of model scripts to include
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return Nothing
#'
#' @export
#'
fill_models <- function(tree = dirtree(), model = models(), quiet = FALSE){
  mods <- sapply(write_model, model, tree = tree, quiet = quiet)
}


#' @title Prepare all of the data options for 
#'
#' @description Create a list of lists of control options for the data
#'
#' @param n_future_moons integer value for the number of future moons to add
#'
#' @param fdate date from which future is defined, typically today's date.
#'
#' @param m_save logical if the moons data should be saved out
#'
#' @param m_filename the name of the file for saving moons data
#'
#'
#' @param type "all" or "controls"
#'
#' @param start newmoon number of the first sample to be included. Default 
#'   value is \code{217}, corresponding to 1995-01-01.
#'
#' @param drop_spp species names to drop from the table
#'
#' @param min_plots minimum number of plots surveyed for a survey to be used
#'
#' @param level for get_rodent_data, automatically set by type
#'
#' @param treatment for get_rodent_data, automatically set by type
#'
#' @param length for get_rodent_data, automatically set by type
#'
#' @param output for get_rodent_data, automatically set by type
#'
#' @param r_save logical if the data should be saved out
#'
#' @param r_filename the name of the file to save the rodent data in
#'
#'
#' @param historical logical indicator whether or historical covariates are
#'   to be included
#'
#' @param forecasts logical indicator whether or forecasted covariates are
#'   to be included
#'
#' @param yr year of today
#'
#' @param lead_time number of moons into the future the rodents are forecast
#'
#' @param min_lag the minimum lag time used in any model
#'
#' @param fcast_nms newmoon numbers to be forecast for covariates
#'
#' @param nfcnm number of forecast newmoons for covariates
#'
#' @param c_save logical if the covariate data should be saved out
#'
#' @param c_filename the name of the covariate file for the saving
#'
#' @return control options list for covariates
#'
#'
#' @return list of control options lists
#'
#' @export
#'
all_data_options <- function(n_future_moons = 0, fdate = today(), 
                             m_save = TRUE, m_filename = "moons.csv",
                             type = NULL, start = 217, drop_spp = "PI", 
                             min_plots = 24, level = "Site", 
                             treatment = NULL, length = "all",
                             output = "abundance", r_save = TRUE, 
                             r_filename = "all.csv",
                             historical = TRUE, forecasts = TRUE, 
                             yr = as.numeric(format(today(), "%Y")),
                             lead_time = 12, min_lag = 6, 
                             fcast_nms = NULL, nfcnm = 0,
                             c_save = TRUE, c_filename = "covariates.csv"){

  moons_opts <- moons_options(n_future_moons = n_future_moons, fdate = fdate,
                              save = m_save, filename = m_filename)
  rodents_opts <- rodents_options(type = type, start = start, 
                                  drop_spp = drop_spp, min_plots = min_plots, 
                                  level = level, treatment = treatment, 
                                  length = length, output = output,
                                  save = r_save, filename = r_filename)
  covs_opts <- covariates_options(historical = historical, 
                                  forecasts = forecasts, fdate = fdate, 
                                  yr = yr, start = start,
                                  lead_time = lead_time, min_lag = min_lag, 
                                  fcast_nms = fcast_nms, nfcnm = nfcnm,
                                  save = c_save, filename = c_filename)

  list("moons_options" = moons_opts, "rodents_options" = rodents_opts,
       "covariates_options" = covs_opts)

}
