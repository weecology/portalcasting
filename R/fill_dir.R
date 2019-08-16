#' @title Fill a forecasting directory with basic components.
#'
#' @description Fill the forecasting directory with basic components.
#'
#' @details Arguments input directly here take precedence over those in the 
#'  \code{downloads} \code{list}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
#'
#' @param min_lag \code{integer} (or integer \code{numeric}) of the minimum 
#'  covariate lag time used in any model.
#'
#' @param cast_date \code{Date} from which future is defined (the origin of
#'  the cast). In the recurring forecasting, is set to today's date
#'  using \code{\link{Sys.Date}}.
#'
#' @param start_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the first sample to be included. Default value is \code{217}, 
#'  corresponding to \code{1995-01-01}.
#'
#' @param confidence_level \code{numeric} confidence level used in 
#'   summarizing model output. Must be between \code{0} and \code{1}.
#'
#' @param hist_covariates \code{logical} indicator of whether or not 
#'  historical covariates are to be included.
#'
#' @param cast_covariates \code{logical} indicator whether or not -casted 
#'  covariates are to be included.
#'
#' @param raw_path_archive \code{character} value of the path to the archive
#'  within the raw sub folder.
#' 
#' @param raw_path_data \code{character} value indicating the folder path
#'  to the data within the \code{raw} subdirectory but above the files. A 
#'  standard portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so \code{raw_path_data = "PortalData"} (as
#'  \code{"raw/"} is implied). 
#' 
#' @param raw_path_predictions \code{character} value of the path to the
#'  predictions folder within the raw sub folder (via the archive).  
#' 
#' @param raw_cov_cast_file \code{character} value of the path to the
#'  covariate cast file within \code{raw_path_archive}.
#'
#' @param raw_path_cov_cast \code{character} value of the path to the folder
#'  that holds any downloaded covariate cast files within the raw sub folder.
#'
#' @param raw_moons_file \code{character} value indicating the path
#'  to the moons data file within \code{raw_path_data}. A standard 
#'  portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so 
#'  \code{raw_moons_file = "Rodents/moon_dates.csv"}.
#'
#' @param source_name \code{character} value for the name to give the 
#'   covariate forecast. Currently is \code{"current_archive"}. Previous to
#'   \code{"current_archive"}, the data were retroactively filled in and are 
#'   given the source name \code{"retroactive"}.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'   cast covariates should be appended to the historical casts for the
#'   purposes of hindcasting later.
#'
#' @param controls_m Additional controls for models not in the prefab set. 
#'  \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariates} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariates = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param controls_r Control \code{list} (from 
#'  \code{\link{rodents_control}}) or \code{list} of control \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_control}} for details. 
#'
#' @param control_cdl \code{list} of specifications for the download, which
#'  are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param downloads \code{list} of arguments to pass to \code{\link{download}}
#'  for raw file downloading. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the information or not (and thus just the tidy messages). 
#'
#' @param save \code{logical} indicator controlling if the output should 
#'   be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param filename_moons \code{character} name of the file for saving the 
#'  data output.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @return All \code{fill_} functions return \code{NULL}.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
#'   fill_raw()
#'   fill_predictions()
#'   fill_models()
#'   fill_data()
#'  }
#'
#' @export
#'
fill_dir <- function(main = ".", models = prefab_models(), end_moon = NULL, 
                     lead_time = 12, cast_date = Sys.Date(),
                     start_moon = 217, 
                     confidence_level = 0.9, 
                     hist_covariates = TRUE, cast_covariates = TRUE,
                     raw_path_archive = "portalPredictions",
                     raw_path_data = "PortalData",
                     raw_path_predictions = "portalPredictions/predictions",
                     raw_cov_cast_file = "data/covariate_casts.csv",
                     raw_path_cov_cast = "cov_casts", 
                     raw_moons_file = "Rodents/moon_dates.csv",
                     source_name = "current_archive",
                     append_cast_csv = TRUE, controls_m = NULL,
                     controls_r = rodents_controls(),
                     control_cdl = list(),
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     quiet = FALSE, verbose = FALSE, save = TRUE,
                     overwrite = TRUE, filename_moons = "moon_dates.csv",
                     filename_cov = "covariates.csv", 
                     filename_meta = "metadata.yaml", cleanup = TRUE){

  pass_and_call(fill_raw)
  pass_and_call(fill_predictions)
  pass_and_call(fill_models)
  pass_and_call(fill_data, min_lag = 6)
}

#' @rdname fill_dir
#'
#' @export
#'
fill_data <- function(main = ".", end_moon = NULL, 
                      lead_time = 12, min_lag = 6, cast_date = Sys.Date(),
                      start_moon = 217, 
                      confidence_level = 0.9, 
                      hist_covariates = TRUE, cast_covariates = TRUE,
                      raw_path_archive = "portalPredictions",
                      raw_path_data = "PortalData",
                      raw_cov_cast_file = "data/covariate_casts.csv",
                      raw_path_cov_cast = "cov_casts", 
                      raw_moons_file = "Rodents/moon_dates.csv",
                      source_name = "current_archive",
                      append_cast_csv = TRUE, 
                      controls_r = rodents_controls(),
                      control_cdl = list(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, save = TRUE,
                      overwrite = TRUE, 
                      filename_moons = "moon_dates.csv",
                      filename_cov = "covariates.csv",
                      filename_meta = "metadata.yaml",
                      cleanup = TRUE){

  raw_data_present <- verify_raw_data(raw_path_data, main)
  if(!raw_data_present){
    fill_raw(downloads, main, quiet, cleanup)
  }
  messageq("Adding data files to data subdirectory", quiet)
  d_m <- pass_and_call(prep_moons)  
  d_r <-  pass_and_call(prep_rodents, moons = d_m)
  d_c <- pass_and_call(prep_covariates, moons = d_m)
  pass_and_call(prep_metadata, moons = d_m, rodents = d_r, covariates = d_c)
  NULL
}

#' @rdname fill_dir
#'
#' @export
#'
fill_models <- function(main = ".", models = prefab_models(), 
                        controls_m = NULL, quiet = FALSE, 
                        overwrite = TRUE){
  return_if_null(models)
  controls_m <- model_script_controls(models, controls_m)
  messageq("Adding models to models subdirectory:", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    pass_and_call(write_model, control = controls_m[[models[i]]])
  } 
  NULL
}

#' @rdname fill_dir
#'
#' @export
#'
fill_predictions <- function(main = ".", raw_path_predictions = NULL, 
                             quiet = FALSE, verbose = FALSE, 
                             overwrite = TRUE){
  messageq("filling predictions folder", quiet)
  local_raw_folder <- paste0("raw/", raw_path_predictions)
  raw_folder <- file_paths(main, local_paths = local_raw_folder)
  pfiles <- list.files(raw_folder)
  raw_files_local <- paste0(local_raw_folder, "/", pfiles)
  raw_files <- file_paths(main, local_paths = raw_files_local)
  final_folder <- sub_paths(main, "predictions")
  fc <- file.copy(raw_files, final_folder, overwrite)
  messageq(fill_predictions_message(pfiles, fc, verbose))
  NULL
}

#' @rdname fill_dir
#'
#' @export
#'
fill_raw <- function(main = ".", 
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     quiet = FALSE, cleanup = TRUE){
  return_if_null(downloads)
  if(list_depth(downloads) == 1){
    downloads <- list(downloads)
  }
  messageq("Downloading raw files...", quiet)
  ndl <- length(downloads)
  for(i in 1:ndl){
    downloads[[i]]$cleanup <- ifnull(downloads[[i]]$cleanup, cleanup)
    downloads[[i]]$main <- ifnull(downloads[[i]]$main, main)
    downloads[[i]]$quiet <- ifnull(downloads[[i]]$quiet, quiet)
    downloads[[i]]$specific_sub <- "raw"
    do.call(download, downloads[[i]])
  }
  NULL
}

#' @title Create the final message for filling predictions
#'
#' @description Create the final message to be used in 
#'  \code{\link{fill_predictions}} that relays the number, and optionally the
#'  specific names, of the files moved.
#'
#' @param files \code{character} vector of the base file names of the files
#'  that may or may not have been moved.
#'
#' @param movedTF \code{logical} vector indicating if each of the files in 
#'  \code{files} has been moved or not.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the files and whether they were moved or not.
#'
#' @return A \code{character} value of the message to be printed (if desired).
#'
#' @examples
#'  fill_predictions_message("xx.csv", TRUE)
#'  fill_predictions_message(c("xx.csv", "yy.R"), c(TRUE, FALSE), 
#'                           verbose = TRUE)
#'
#' @export
#'
fill_predictions_message <- function(files = NULL, movedTF = NULL, 
                                     verbose = FALSE){
  return_if_null(files)
  moved <- files[movedTF]
  not_moved <- files[!movedTF]
  n_moved <- length(moved)
  n_not_moved <- length(not_moved)
  msg <- NULL
  if(verbose){
    if(n_moved > 0){
      msg <- c(msg, "moved:", moved)
    }
    if(n_not_moved > 0){
      msg <- c(msg, "not moved: ", not_moved)
    }  
  }
  c(msg, paste0(n_moved, " predictions files moved, ", n_not_moved, " not"))
}



