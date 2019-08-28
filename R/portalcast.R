#' @title Cast Portal rodents models
#'
#' @description Cast the Portal rodent population data using the (updated if 
#'  needed) data and models in a portalcasting directory. \cr
#'  \code{portalcast} wraps around \code{cast} to allow multiple runs of 
#'  multiple models, with data preparation as needed between runs occurring
#'  via \code{prep_data}. \cr \cr
#'  \code{prep_data} makes sure that the data subdirectory has the updated 
#'  files for a casting run, using the \code{metadata.yaml} file to verify 
#'  validity. If data are out of date, runs \code{\link{fill_data}}. \cr \cr
#'  \code{cast} runs a single cast of multiple models, using the data as
#'  prepared by \code{prep_data}.
#'
#' @details Multiple models can be run together on the same data and 
#'  multiple runs of a model (e.g., with a rolling origin) can be initiated
#'  together using \code{portalcast} and \code{cast} runs the individual
#'  instances.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to 
#'  include.
#'
#' @param ensemble \code{logical} indicator if the ensemble should be added.
#'
#' @param end_moons,end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number(s) of the last sample(s) to be included. Default value is 
#'  \code{NULL}, which equates to the most recently included sample. \cr
#'  \strong{\code{end_moons} allows for multiple moons, \code{end_moon}
#'  can only be one value}.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
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
#' @param directory \code{character} value of the directory name.
#' 
#' @param raw_data \code{character} value indicating the name of the raw
#'  data directory. A standard portalcasting directory downloads the raw data
#'  files into from the PortalData repository, so 
#'  \code{raw_data = "PortalData"}.
#'
#' @param source_name \code{character} value for the name to give the 
#'   covariate forecast. Currently is \code{"current_archive"}. Previous to
#'   \code{"current_archive"}, the data were retroactively filled in and are 
#'   given the source name \code{"retroactive"}.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'   cast covariates should be appended to the historical casts for later use.
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
#' @param controls_r Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details.  
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
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#
#' @param data_sets \code{character} values of the rodent data sets that
#'  are needed, used to enforce certain arguments in data creation (see 
#'  \code{\link{prep_rodents_table}}). The prefab sets currently include 
#'  \code{"all"}, \code{"controls"}, \code{"all_interp"},
#'  and \code{"controls_interp"} (\code{\link{prefab_data_sets}}).
#'  
#' 
#' @return Results are saved to files, \code{NULL} is returned.
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   portalcast()
#'   prep_data()
#'   cast()
#'  }
#'
#' @export
#'
portalcast <- function(main = ".", models = prefab_models(), ensemble = FALSE,
                       data_sets = prefab_data_sets(), end_moons = NULL, 
                       lead_time = 12, cast_date = Sys.Date(),
                       start_moon = 217, 
                       confidence_level = 0.95, 
                       hist_covariates = TRUE, cast_covariates = TRUE,
                       directory = "portalPredictions",
                       raw_data = "PortalData",
                       source_name = "current_archive",
                       append_cast_csv = TRUE, controls_m = NULL,
                       controls_r = rodents_controls(),
                       control_cdl = climate_dl_control(),
                       downloads = zenodo_downloads(c("1215988", "833438")), 
                       quiet = FALSE, verbose = FALSE, 
                       save = TRUE, overwrite = TRUE, 
                       filename_moons = "moon_dates.csv",
                       filename_config = "dir_config.yaml", 
                       filename_cov = "covariates.csv", 
                       filename_cov_casts = "covariate_casts.csv",
                       filename_meta = "metadata.yaml", cleanup = TRUE, 
                       arg_checks = TRUE){
  check_args(arg_checks)
  portalcast_welcome(quiet = quiet)
  messageq("Preparing directory for casting", quiet)
  messageq("---------------------------------------------------------", quiet)
  verify_models(main = main, models = models, quiet = quiet, 
                arg_checks = arg_checks)
  verify_raw_data(main = main, raw_data = raw_data, 
                  arg_checks = arg_checks)
  min_lag <- extract_min_lag(models = models, controls_m = controls_m,
                             arg_checks = arg_checks)
  moons <- read_moons(main = main, arg_checks = arg_checks)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                            arg_checks = arg_checks)
  end_moons <- ifnull(end_moons, last_moon)
  nend_moons <- length(end_moons)
  for(i in 1:nend_moons){
    prep_data(main = main, data_sets = data_sets, end_moon = end_moons[i], 
              lead_time = lead_time, 
              min_lag = min_lag, cast_date = cast_date, 
              start_moon = start_moon, confidence_level = confidence_level, 
              hist_covariates = hist_covariates, 
              cast_covariates = cast_covariates,
              directory = directory,
              raw_data = raw_data,
              append_cast_csv = append_cast_csv, controls_r = controls_r,
              controls_m = controls_m, 
              control_cdl = control_cdl, downloads = downloads, 
              quiet = quiet, verbose = verbose, save = save,
              overwrite = overwrite, filename_moons = filename_moons,
              filename_cov = filename_cov, filename_meta = filename_meta,
              filename_config = filename_config,
              filename_cov_casts = filename_cov_casts,
              cleanup = cleanup, arg_checks = arg_checks)
    cast(main = main, models = models, ensemble = ensemble,
         cast_date = cast_date, moons = moons, 
         end_moon = end_moons[i], raw_data = raw_data,
         controls_r = controls_r, 
         confidence_level = confidence_level, quiet = quiet, 
         verbose = verbose, cleanup = cleanup, arg_checks = TRUE)
  }
  portalcast_goodbye(quiet = quiet)
} 


#' @rdname portalcast
#'
#' @export
#'
cast <- function(main = ".", models = prefab_models(), ensemble = FALSE,
                 cast_date = Sys.Date(), moons = NULL, 
                 end_moon = NULL, raw_data = "PortalData",
                 controls_r = rodents_controls(), confidence_level = 0.95, 
                 quiet = FALSE, verbose = FALSE, cleanup = TRUE, 
                 arg_checks = TRUE){
  moons <- ifnull(moons, read_moons(main = main))
  check_args(arg_checks)
  clear_tmp(main = main, quiet = quiet, cleanup = cleanup,
            arg_checks = arg_checks)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)

  msg1 <- "---------------------------------------------------------"
  msg2 <- paste0("Running models for forecast origin newmoon ", end_moon)
  messageq(c(msg1, msg2), quiet)

  models_scripts <- models_to_cast(main = main, models = models,
                                   arg_checks = arg_checks)
  sapply(models_scripts, source)
  messageq("---------------------------------------------------------", quiet)
  clear_tmp(main = main, quiet = quiet, verbose = verbose, cleanup = cleanup,
            arg_checks = arg_checks)
}


#' @rdname portalcast
#'
#' @export
#'
prep_data <- function(main = ".", end_moon = NULL, models = prefab_models(), 
                      data_sets = prefab_data_sets(),
                      lead_time = 12, min_lag = 6, cast_date = Sys.Date(),
                      start_moon = 217, 
                      confidence_level = 0.95, 
                      hist_covariates = TRUE, cast_covariates = TRUE,
                      directory = "portalPredictions",
                      raw_data = "PortalData",
                      source_name = "current_archive",
                      append_cast_csv = TRUE, 
                      controls_r = rodents_controls(), controls_m = NULL, 
                      control_cdl = climate_dl_control(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, 
                      save = TRUE, overwrite = TRUE, 
                      filename_moons = "moon_dates.csv",
                      filename_cov = "covariates.csv", 
                      filename_config = filename_config,
                      filename_cov_casts = "covariate_casts.csv",
                      filename_meta = "metadata.yaml", cleanup = TRUE, 
                      arg_checks = TRUE){
  check_args(arg_checks)
  messageq("Readying data", quiet)
  metadata <- read_metadata(main = main, arg_checks = arg_checks)
  moons <- read_moons(main = main, arg_checks = arg_checks)
  date_current <- metadata$cast_date == Sys.Date()
  moon_match <- metadata$rodent_cast_moons[1] == (end_moon + 1)
  moon_match <- ifelse(length(moon_match) == 0, FALSE, moon_match)
  last_moon <- last_moon(main = main, moons = moons, date = cast_date,
                            arg_checks = arg_checks)
  end_moon <- ifnull(end_moon, last_moon)
  old_moon <- !(end_moon == last_moon)
  if (!date_current | !moon_match | old_moon){
    fill_data(main = main, models = models, 
             end_moon = end_moon, lead_time = lead_time, 
             cast_date = cast_date, start_moon = start_moon, 
             confidence_level = confidence_level, 
             hist_covariates = hist_covariates, 
             cast_covariates = cast_covariates,
             directory = directory,
             raw_data = raw_data,
             source_name = source_name,
             append_cast_csv = append_cast_csv, controls_m = controls_m, 
             controls_r = controls_r, control_cdl = control_cdl, 
             downloads = downloads, quiet = quiet, verbose = verbose, 
             save = save, overwrite = overwrite, 
             filename_moons = filename_moons, filename_cov = filename_cov, 
             filename_meta = filename_meta, cleanup = cleanup, 
             filename_config = filename_config,
             filename_cov_casts = filename_cov_casts,
             arg_checks = arg_checks)
  }
  messageq("---------------------------------------------------------", quiet)
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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
models_to_cast <- function(main = ".", models = prefab_models(), 
                           arg_checks = TRUE){
  check_args(arg_checks)
  models_path <- models_path(main = main, arg_checks = arg_checks)
  file_names <- paste0(models, ".R")
  torun <- (list.files(models_path) %in% file_names)
  torun_paths <- list.files(models_path, full.names = TRUE)[torun] 
  normalizePath(torun_paths)
}