#' @title Fill a forecasting directory with basic components.
#'
#' @description Fill the directory with foundational components. \cr \cr
#'  \code{fill_dir} combines \code{fill_raw}, \code{fill_casts},
#'   \code{fill_models}, and \code{fill_data}.
#'  \code{fill_raw} downloads the raw data and archive and updates the 
#'   directory configuration metadata accordingly. \cr \cr
#'  \code{fill_casts} moves the historic casts from the archive into the 
#'   current directory. \cr \cr
#'  \code{fill_models} writes out the model scripts to the models
#'   subdirectory. \cr \cr
#'  \code{fill_data} prepares model-ready data from the raw data.
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
#'  covariate forecast. Currently is \code{"current_archive"}. Previous to
#'  \code{"current_archive"}, the data were retroactively filled in and are 
#'  given the source name \code{"retroactive"}.
#'
#' @param append_cast_csv \code{logical} indicator controlling if the new 
#'  cast covariates should be appended to the historical casts later use.
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
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param save \code{logical} indicator controlling if the output should 
#'  be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing
#'  files should be updated (most users should leave as \code{TRUE}).
#'
#' @param filename_moons \code{character} name of the file for saving the 
#'  moons data.
#'
#' @param filename_cov_casts \code{character} filename for saving the 
#'  covariate casts output.
#'
#' @param filename_meta \code{character} filename for saving the metadata.
#'
#' @param filename_config \code{character} filename of the directory
#'  configuration YAML.
#'
#' @param filename_cov \code{character} filename for saving the output.
#'
#' @param cleanup \code{logical} indicator if any files put into the tmp
#'  subdirectory should be removed at the end of the process. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return All \code{fill_} functions return \code{NULL}. 
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_dir()
#'   fill_raw()
#'   fill_casts()
#'   fill_models()
#'   fill_data()
#'  }
#'
#' @name fill_directory
#'
NULL

#' @rdname fill_directory
#'
#' @export
#'
fill_dir <- function(main = ".", models = prefab_models(), 
                     end_moon = NULL, 
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
                     quiet = FALSE, verbose = FALSE, save = TRUE,
                     overwrite = TRUE, filename_moons = "moon_dates.csv",
                     filename_config = "dir_config.yaml", 
                     filename_cov = "covariates.csv", 
                     filename_cov_casts = "covariate_casts.csv",
                     filename_meta = "metadata.yaml", 
                     cleanup = TRUE, 
                     arg_checks = TRUE){
  check_args(arg_checks)
  messageq("Filling directory with standard content", quiet)
  fill_raw(main = main, downloads = downloads, quiet = quiet, 
           cleanup = cleanup, arg_checks = arg_checks)
  fill_casts(main = main, directory = directory, quiet = quiet,  
             verbose = verbose, overwrite = overwrite,
             arg_checks = arg_checks)
  fill_models(main = main, models = models, controls_m = controls_m, 
              quiet = quiet, verbose = verbose, overwrite = overwrite,
              arg_checks = arg_checks)
  fill_data(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            hist_covariates = hist_covariates, 
            cast_covariates = cast_covariates,
            directory = directory,
            raw_data = raw_data,
            source_name = source_name,
            append_cast_csv = append_cast_csv, controls_r = controls_r,
            controls_m = controls_m, control_cdl = control_cdl, 
            downloads = downloads, 
            quiet = quiet, verbose = verbose, save = save,
            overwrite = overwrite, filename_moons = filename_moons,
            filename_cov = filename_cov, filename_meta = filename_meta,
            filename_cov_casts = filename_cov_casts, 
            filename_config = filename_config,
            cleanup = cleanup, arg_checks = arg_checks)
}



#' @rdname fill_directory
#'
#' @export
#'
fill_data <- function(main = ".", models = prefab_models(),
                      end_moon = NULL, lead_time = 12,
                      cast_date = Sys.Date(), start_moon = 217, 
                      confidence_level = 0.95, 
                      hist_covariates = TRUE, cast_covariates = TRUE,
                      directory = "portalPredictions",
                      raw_data = "PortalData",
                      source_name = "current_archive",
                      append_cast_csv = TRUE, 
                      controls_r = rodents_controls(), 
                      controls_m = NULL,
                      control_cdl = climate_dl_control(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, save = TRUE,
                      overwrite = TRUE, 
                      filename_moons = "moon_dates.csv",
                      filename_cov = "covariates.csv",
                      filename_cov_casts = "covariate_casts.csv",
                      filename_meta = "metadata.yaml",
                      filename_config = "dir_config.yaml", 
                      cleanup = TRUE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  min_lag <- extract_min_lag(models = models, controls_m = controls_m, 
                             quiet = quiet, arg_checks = arg_checks)
  data_sets <- extract_data_sets(models = models, controls_m = controls_m, 
                                 quiet = quiet, arg_checks = arg_checks)

  raw_data_present <- verify_raw_data(main = main, raw_data = raw_data, 
                                      arg_checks = arg_checks)
  if(!raw_data_present){
    fill_raw(main = main, downloads = downloads, quiet = quiet, 
             cleanup = cleanup, arg_checks = arg_checks)
  }
  messageq(" -Adding data files to data subdirectory", quiet)
  data_m <- prep_moons(main = main, lead_time = lead_time, 
                       cast_date = cast_date, raw_data = raw_data,
                       quiet = quiet, verbose = verbose,
                       save = save, overwrite = overwrite, 
                       filename_moons = filename_moons, 
                       arg_checks = arg_checks)
  data_r <- prep_rodents(main = main, moons = data_m, 
                         data_sets = data_sets, end_moon = end_moon, 
                         start_moon = start_moon, controls_r = controls_r,
                         quiet = quiet, verbose = verbose, save = save, 
                         overwrite = overwrite, arg_checks = arg_checks)
  data_c <- prep_covariates(main = main, moons = data_m, end_moon = end_moon, 
                            lead_time = lead_time, min_lag = min_lag, 
                            cast_date = cast_date, 
                            hist_covariates = hist_covariates, 
                            cast_covariates = cast_covariates,
                            directory = directory,
                            source_name = source_name,
                            append_cast_csv = append_cast_csv, 
                            control_cdl = control_cdl,
                            quiet = quiet, save = save, overwrite = overwrite, 
                            filename_cov = filename_cov,
                            arg_checks = arg_checks)
  prep_metadata(main = main, models = models,
                data_sets = data_sets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_r = controls_r,  
                quiet = quiet, save = save, 
                overwrite = overwrite, filename_meta = filename_meta, 
                filename_config = filename_config,
                arg_checks = arg_checks)

  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_models <- function(main = ".", models = prefab_models(), 
                        controls_m = NULL, quiet = FALSE, verbose = FALSE, 
                        overwrite = TRUE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  controls_m <- model_script_controls(models = models, 
                                      controls_m = controls_m,
                                      quiet = quiet, arg_checks = arg_checks)
  messageq(" -Writing model scripts", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, verbose = verbose, 
                overwrite = overwrite, control = controls_m[[models[i]]], 
                arg_checks = arg_checks)
  } 
  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_casts <- function(main = ".", directory = "portalPredictions",
                       quiet = FALSE, verbose = FALSE, overwrite = TRUE, 
                       arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  messageq(" -Filling casts folder with files from archive", quiet)
  path_casts <- paste0(directory, "/casts")
  archive <- file_path(main = main, sub = "raw", files = path_casts,
                       arg_checks = arg_checks)
  arch_files <- list.files(archive, full.names = TRUE)
  if(length(arch_files) == 0){
    path_casts <- paste0(directory, "/predictions")
    archive <- file_path(main = main, sub = "raw", files = path_casts,
                         arg_checks = arg_checks)
    arch_files <- list.files(archive, full.names = TRUE)
  }
  arch_files_local <- paste0(path_casts, arch_files)
  casts_folder <- casts_path(main = main, arg_checks = arg_checks)
  fc <- file.copy(arch_files, casts_folder, overwrite)
  cast_meta <- read_cast_metadata(main = main, quiet = quiet, 
                                  arg_checks = arg_checks)
  fill_casts_message(files = arch_files, movedTF = fc, quiet = !verbose,
                     verbose = verbose, arg_checks = arg_checks)
  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_raw <- function(main = ".", 
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     quiet = FALSE, cleanup = TRUE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(downloads)
  if(list_depth(downloads) == 1){
    downloads <- list(downloads)
  }
  messageq(" -Downloading raw files", quiet)
  ndl <- length(downloads)
  dl_vers <- rep(NA, ndl)
  for(i in 1:ndl){
    downloads[[i]]$cleanup <- ifnull(downloads[[i]]$cleanup, cleanup)
    downloads[[i]]$main <- ifnull(downloads[[i]]$main, main)
    downloads[[i]]$quiet <- ifnull(downloads[[i]]$quiet, quiet)
    downloads[[i]]$sub <- "raw"
    dl_vers[i] <- do.call(download, downloads[[i]])
  }
  update_directory_config(main = main, downloads_versions = dl_vers,
                          quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}
