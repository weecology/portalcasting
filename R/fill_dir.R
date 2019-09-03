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
#' @param controls_models Additional controls for models not in the prefab 
#'  set. \cr 
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
#' @param controls_rodents Control \code{list} or \code{list} of \code{list}s 
#'  (from \code{\link{rodents_controls}}) specifying the structuring of the 
#'  rodents tables. See \code{\link{rodents_controls}} for details. 
#'
#' @param control_climate_dl \code{list} of specifications for the download, 
#'  which are sent to \code{\link{NMME_urls}} to create the specific URLs. See
#'  \code{\link{climate_dl_control}}.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
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
                     end_moon = NULL, start_moon = 217,
                     lead_time = 12, confidence_level = 0.95, 
                     cast_date = Sys.Date(), controls_models = NULL,
                     controls_rodents = rodents_controls(),
                     control_climate_dl = climate_dl_control(),
                     control_files = files_control(),
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  messageq("Filling directory with standard content", quiet)
  fill_raw(main = main, downloads = downloads, quiet = quiet, 
           control_files = control_files, arg_checks = arg_checks)
  fill_casts(main = main, quiet = quiet, verbose = verbose, 
             control_files = control_files, arg_checks = arg_checks)
  fill_models(main = main, models = models, controls_models = controls_models, 
              quiet = quiet, verbose = verbose, control_files = control_files,
              arg_checks = arg_checks)
  fill_data(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_rodents = controls_rodents,
            controls_models = controls_models, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, control_files = control_files,
            quiet = quiet, verbose = verbose, arg_checks = arg_checks)
}



#' @rdname fill_directory
#'
#' @export
#'
fill_data <- function(main = ".", models = prefab_models(),
                      end_moon = NULL,start_moon = 217, lead_time = 12,
                      confidence_level = 0.95, cast_date = Sys.Date(), 
                      controls_models = NULL,
                      controls_rodents = rodents_controls(), 
                      control_climate_dl = climate_dl_control(),
                      control_files = files_control(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)

  min_lag <- extract_min_lag(models = models, 
                             controls_models = controls_models, 
                             quiet = quiet, arg_checks = arg_checks)
  data_sets <- extract_data_sets(models = models, 
                                 controls_models = controls_models, 
                                 quiet = quiet, arg_checks = arg_checks)

  raw_data_present <- verify_raw_data(main = main, 
                                      raw_data = control_files$raw_data, 
                                      arg_checks = arg_checks)
  if(!raw_data_present){
    fill_raw(main = main, downloads = downloads, quiet = quiet, 
             control_files = control_files, arg_checks = arg_checks)
  }
  messageq(" -Adding data files to data subdirectory", quiet)
  data_m <- prep_moons(main = main, lead_time = lead_time, 
                       cast_date = cast_date, 
                       quiet = quiet, verbose = verbose,
                       control_files = control_files, 
                       arg_checks = arg_checks)
  data_r <- prep_rodents(main = main, moons = data_m, 
                         data_sets = data_sets, end_moon = end_moon, 
                         start_moon = start_moon, 
                         controls_rodents = controls_rodents,
                         quiet = quiet, verbose = verbose, 
                         control_files = control_files, 
                         arg_checks = arg_checks)
  data_c <- prep_covariates(main = main, moons = data_m, end_moon = end_moon, 
                            lead_time = lead_time, min_lag = min_lag, 
                            cast_date = cast_date, 
                            control_climate_dl = control_climate_dl,
                            quiet = quiet, control_files = control_files,
                            arg_checks = arg_checks)
  prep_metadata(main = main, models = models,
                data_sets = data_sets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_rodents = controls_rodents, quiet = quiet, 
                control_files = control_files, arg_checks = arg_checks)

  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_models <- function(main = ".", models = prefab_models(), 
                        controls_models = NULL, 
                        control_files = files_control(), quiet = FALSE, 
                        verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  controls_models <- model_script_controls(models = models, 
                                           controls_models = controls_models,
                                           quiet = quiet, 
                                           arg_checks = arg_checks)
  messageq(" -Writing model scripts", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, verbose = verbose, 
                control_files = control_files, 
                control_model = controls_models[[models[i]]], 
                arg_checks = arg_checks)
  } 
  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_casts <- function(main = ".", control_files = files_control(),
                       quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  directory <- control_files$directory
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
  fc <- file.copy(arch_files, casts_folder, control_files$overwrite)
  casts_meta <- read_casts_metadata(main = main, quiet = quiet, 
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
                     control_files = files_control(), quiet = FALSE, 
                     arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(downloads)
  if(list_depth(downloads) == 1){
    downloads <- list(downloads)
  }
  messageq(" -Downloading raw files", quiet)
  ndl <- length(downloads)
  dl_vers <- rep(NA, ndl)
  for(i in 1:ndl){
    downloads[[i]]$cleanup <- ifnull(downloads[[i]]$cleanup, 
                                     control_files$cleanup)
    downloads[[i]]$main <- ifnull(downloads[[i]]$main, main)
    downloads[[i]]$quiet <- ifnull(downloads[[i]]$quiet, quiet)
    downloads[[i]]$sub <- "raw"
    dl_vers[i] <- do.call(download, downloads[[i]])
  }
  update_directory_config(main = main, downloads_versions = dl_vers,
                          quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}
