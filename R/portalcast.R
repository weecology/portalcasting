#' @title Forecast or hindcast Portal rodents models
#'
#' @description Forecast or hindcast the Portal rodent population data using
#'  the data and models in a portalcasting directory. 
#'
#' @details Multiple models can be run together on the same data and 
#'  multiple runs of a model (e.g., with a rolling origin) can be initiated
#'  together. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param ensemble \code{logical} indicator if the ensemble should be added.
#'
#' @param end_moons \code{integer} (or integer \code{numeric}) newmoon numbers 
#'  of the last samples to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample. 
#'
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the
#'  number of timesteps forward a cast will cover.
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
#' @param raw_traps_file \code{character} value indicating the path
#'  to the trapping data file within \code{raw_path_data}. A standard 
#'  portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so 
#'  \code{raw_traps_file = "Rodents/Portal_rodent_trapping.csv"}.
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
#' @return Results are saved to files, \code{NULL} is returned.
#'
#' @examples
#' \donttest{
#'  setup_dir()
#'  portalcast()
#' }
#'
#' @export
#'
portalcast <- function(main = ".", models = prefab_models(), ensemble = TRUE,
                       end_moons = NULL, 
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
                       raw_traps_file = "Rodents/Portal_rodent_trapping.csv",
                       source_name = "current_archive",
                       append_cast_csv = TRUE, controls_m = NULL,
                       controls_r = rodents_controls(),
                       control_cdl = list(),
                       downloads = zenodo_downloads(c("1215988", "833438")), 
                       quiet = FALSE, verbose = FALSE, 
                       save = TRUE, overwrite = TRUE, 
                       filename_moons = "moon_dates.csv",
                       filename_cov = "covariates.csv", 
                       filename_meta = "metadata.yaml", cleanup = TRUE){
  pass_and_call(portalcast_welcome)
  pass_and_call(verify_models)
  min_lag <- pass_and_call(extract_min_lag)
  last_moon <- pass_and_call(last_newmoon)
  end_moons <- ifnull(end_moons, last_moon)
  end_moons <- sort(end_moons, decreasing = TRUE)
  nend_moons <- length(end_moons)
  for(i in 1:nend_moons){
    pass_and_call(prep_data, min_lag = min_lag, end_moon = end_moons[i])
    #pass_and_call(cast)
  }
  pass_and_call(portalcast_goodbye)
} 



cast <- function(main = ".", moons = prep_moons(main = main), 
                          end_moon = NULL, raw_path_data = "PortalData",
                          raw_traps_file = 
                            "Rodents/Portal_rodent_trapping.csv",
                          controls_r = rodents_controls()){
  skips <- pass_and_call(check_to_skip)
  if(skips){
    return()
  }
  last_moon <- pass_and_call(last_newmoon)
  end_moon <- ifnull(end_moon, last_moon)
  msg1 <- "##########################################################"
  msg2 <- paste0("Running models for forecast origin newmoon ", end_moon)

#  models_scripts <- pass_and_call(models_to_cast)
#  sapply(models_scripts, source)
#  pass_and_call(combine_forecasts)
#  pass_and_call(add_ensemble)
  messageq("########################################################", quiet)
  pass_and_call(clear_tmp)
}










###############################


#' @title Check if the newmoon should be skipped during a set of casts
#' 
#' @description Newmoons with incomplete surveys are skipped.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) newmoon number 
#'  of the last sample to be included. Default value is \code{NULL}, which 
#'  equates to the most recently included sample.  
#' 
#' @param raw_path_data \code{character} value indicating the folder path
#'  to the data within the \code{raw} subdirectory but above the files. A 
#'  standard portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so \code{raw_path_data = "PortalData"} (as
#'  \code{"raw/"} is implied). 
#' 
#' @param raw_traps_file \code{character} value indicating the path
#'  to the trapping data file within \code{raw_path_data}. A standard 
#'  portalcasting directory downloads the raw data files into 
#'  \code{"raw\PortalData"}, so 
#'  \code{raw_traps_file = "Rodents/Portal_rodent_trapping.csv"}.
#'
#' @param controls_r Control \code{list} (from 
#'  \code{\link{rodents_control}}) or \code{list} of control \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_control}} for details. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#' 
#' @return \code{logical} indicator of if the moon should be skipped.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   check_to_skip()
#'  }
#'
#' @export
#'
check_to_skip <- function(main = ".", moons = prep_moons(main = main), 
                          end_moon = NULL, raw_path_data = "PortalData",
                          raw_traps_file = 
                            "Rodents/Portal_rodent_trapping.csv",
                          controls_r = rodents_controls(), quiet = FALSE){
  if(list_depth(controls_r) == 1){
    controls_r <- list(controls_r)
  }
  ncontrols_r <- length(controls_r)
  last_moon <- pass_and_call(last_newmoon)
  end_moon <- ifnull(end_moon, last_moon)

  lpath <- paste0("raw/", raw_path_data, "/", raw_traps_file)
  trap_path <- file_paths(main, lpath) 
  trap_tab <- read.csv(trap_path, stringsAsFactors = FALSE)

  min_plots <- 1
  min_traps <- 1
  for(i in 1:ncontrols_r){
    min_plots <- max(c(min_plots, controls_r[[i]]$min_plots))
    min_traps <- max(c(min_traps, controls_r[[i]]$min_traps))
  }
  incs <- find_incomplete_censuses(trap_tab, min_plots, min_traps)

  end_period <- moons$period[moons$newmoonnumber == end_moon]

  out <- FALSE
  if (is.na(end_period)){
    out <- TRUE
  } else if (end_period %in% incs$period){
    out <- TRUE
  }
  if (out){
    msg1 <- "##########################################################"
    msg2 <- paste0("Newmoon ", end_moon, " not fully sampled; skipped")
    messageq(c(msg1, msg2, msg1), quiet)
  }
  out

}


#' @title Prepare data subdirectory for a forecast or hindcast run(s)
#'
#' @description Make sure that the data subdirectory has the updated files
#'  for a forecasting run or a set of hindcasting runs. Uses the 
#'  \code{metadata.yaml} file to verify validity. \cr
#'  If data are out of date, runs \code{\link{fill_data}}.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
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
#' @return Data files are updated if needed and \code{NULL} is returned.
#'
#' @examples
#' \donttest{
#'  setup_dir()
#'  prep_data()
#' }
#'
#' @export
#'
prep_data <- function(main = ".", end_moon = NULL, 
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
                      quiet = FALSE, verbose = FALSE, 
                      save = TRUE, overwrite = TRUE, 
                      filename_moons = "moon_dates.csv",
                      filename_cov = "covariates.csv", 
                      filename_meta = "metadata.yaml", cleanup = TRUE){
  messageq("Preparing data...", quiet)
  metadata_path <- file_paths(main, "data/metadata.yaml")
  meta_exist <- file.exists(metadata_path)
  metadata <- yaml.load_file(metadata_path)    
  date_current <- metadata$cast_date == Sys.Date()
  moon_match <- metadata$rodent_cast_newmoons[1] == (end_moon + 1)
  moon_match <- ifelse(length(moon_match) == 0, FALSE, moon_match)
  if (!meta_exist | !date_current | !moon_match){
    pass_and_call(fill_data)
    messageq(" ...data updated", quiet)
  } else{
    messageq(" ...data already up-to-date", quiet)
  }
}


#' @title Determine the paths to the scripts for models to cast with
#'
#' @description Translate a \code{character} vector of model name(s) into a 
#'  \code{character} vector of file path(s) corresponding to the model 
#'  scripts. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @return \code{character} vector of the path(s) of the R script file(s) to 
#'   be run.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   models_to_cast()
#'  }
#'
#' @export
#'
models_to_cast <- function(main = ".", models = prefab_models()){
  models_path <- sub_paths(main, "models")
  file_names <- paste0(models, ".R")
  torun <- (list.files(models_path) %in% file_names)
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 
  normalizePath(torun_paths)
}