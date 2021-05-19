#' @title Fill a portalcasting directory with basic components.
#'
#' @description Fill the directory with foundational components. 
#'             
#'             
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'
#'   create_dir("./portalcasting")
#'   fill_dir("./portalcasting")
#'
#'  }
#'
#' @name fill_directory
#'
NULL

#' @rdname fill_directory
#'
#' @export
#'
fill_dir <- function (main  = ".",
                      PortalData_version = "latest",
                      PortalData_source = "gitub",
                      portalPredictions_version = NULL,
                      portalPredictions_source = "github",
                      quiet = FALSE) {

  messageq("Filling directory with standard content",
           quiet = quiet)

  if (!is.null(PortalData_version)) {

    download_observations(path = resources_path(main = main), 
                          version = PortalData_version,
                          from_zenodo = ifelse(PortalData_source == "zenodo", 
                                               TRUE, FALSE),
                          quiet = quiet)

  }

  if (!is.null(portalPredictions_version)) {

    download_archive(path = resources_path(main = main), 
                     version = portalPredictions_version,
                     source = portalPredictions_source,
                     quiet = quiet)

  }

return()

  fill_casts(main = main, quiet = quiet, verbose = verbose, 
             control_files = control_files, arg_checks = arg_checks)
  fill_fits(main = main, quiet = quiet, verbose = verbose, 
            control_files = control_files, arg_checks = arg_checks)
  fill_models(main = main, models = models, controls_model = controls_model, 
              quiet = quiet, verbose = verbose, control_files = control_files,
              arg_checks = arg_checks)
  fill_data(main = main, models = models, 
            end_moon = end_moon, lead_time = lead_time, 
            cast_date = cast_date, start_moon = start_moon, 
            confidence_level = confidence_level, 
            controls_rodents = controls_rodents,
            controls_model = controls_model, 
            control_climate_dl = control_climate_dl, 
            downloads = downloads, control_files = control_files,
            quiet = quiet, verbose = verbose, arg_checks = arg_checks)
}



#' @rdname fill_directory
#'
#' @export
#'
fill_data <- function(main = ".", models = prefab_models(),
                      end_moon = NULL, start_moon = 217, lead_time = 12,
                      confidence_level = 0.95, cast_date = Sys.Date(), 
                      controls_model = NULL,
                      controls_rodents = rodents_controls(), 
                      control_climate_dl = climate_dl_control(),
                      control_files = files_control(),
                      downloads = zenodo_downloads(c("1215988", "833438")), 
                      quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)

  min_lag <- extract_min_lag(models = models, 
                             controls_model = controls_model, 
                             quiet = quiet, arg_checks = arg_checks)
  data_sets <- extract_data_sets(models = models, 
                                 controls_model = controls_model, 
                                 quiet = quiet, arg_checks = arg_checks)

  fill_raw(main = main, downloads = downloads, only_if_missing = TRUE, 
           quiet = quiet, control_files = control_files, 
           arg_checks = arg_checks)

  messageq(" -Adding data files to data subdirectory", quiet)
  data_m <- prep_moons(main = main, lead_time = lead_time, 
                       cast_date = cast_date, 
                       quiet = quiet, verbose = verbose,
                       control_files = control_files, 
                       arg_checks = arg_checks)
  data_r <- prep_rodents(main = main, moons = data_m, 
                         data_sets = data_sets, end_moon = end_moon, 
                         controls_rodents = controls_rodents,
                         quiet = quiet, verbose = verbose, 
                         control_files = control_files, 
                         arg_checks = arg_checks)
  data_c <- prep_covariates(main = main, moons = data_m, end_moon = end_moon, 
                            start_moon = start_moon, lead_time = lead_time, 
                            min_lag = min_lag, cast_date = cast_date, 
                            control_climate_dl = control_climate_dl,
                            quiet = quiet, control_files = control_files,
                            arg_checks = arg_checks)
  prep_metadata(main = main, models = models,
                data_sets = data_sets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_model = controls_model,
                controls_rodents = controls_rodents, quiet = quiet, 
                control_files = control_files, arg_checks = arg_checks)

  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_models <- function(main = ".", models = prefab_models(), 
                        controls_model = NULL, 
                        control_files = files_control(), quiet = FALSE, 
                        verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  controls_model <- model_controls(models = models, 
                                    controls_model = controls_model,
                                    quiet = quiet, arg_checks = arg_checks)
  messageq(" -Writing model scripts", quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, verbose = verbose, 
                control_files = control_files, 
                control_model = controls_model[[models[i]]], 
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
fill_fits <- function(main = ".", control_files = files_control(),
                      quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  directory <- control_files$directory
  path_fits <- paste0(directory, "/fits")
  fits_folder <- fits_path(main = main, arg_checks = arg_checks)

  archive <- file_path(main = main, sub = "raw", files = path_fits,
                       arg_checks = arg_checks)
  arch_files <- list.files(archive, full.names = TRUE)
  if(length(arch_files) == 0){
    arch_files <- NULL
    fc <- FALSE
  } else{
    messageq(" -Filling fits folder with files from archive", quiet)
    fc <- file.copy(arch_files, fits_folder, control_files$overwrite)
  }
  fill_fits_message(files = arch_files, movedTF = fc, quiet = !verbose,
                    verbose = verbose, arg_checks = arg_checks)
  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_raw <- function(main = ".", 
                     downloads = zenodo_downloads(c("1215988", "833438")), 
                     only_if_missing = FALSE,
                     control_files = files_control(), quiet = FALSE, 
                     arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(downloads)
  mainp <- main_path(main = main, arg_checks = arg_checks)
  verify(paths = mainp, arg_checks = arg_checks)
  if(list_depth(downloads) == 1){
    downloads <- list(downloads)
  }
  ndl <- length(downloads)

  # this is very much patched together here, needs to be generalized
  # we'll want to make it be so any directories can get downloaded, not 
  # just these two
  # and here is also where we'll want to verify versions to not update etc

  raw_data_dir <- control_files$raw_data
  raw_data_path <- file_path(main = main, sub = "raw", files = raw_data_dir, 
                             arg_checks = arg_checks)
  raw_data_pres <- file.exists(raw_data_path)

  directory_dir <- control_files$directory
  directory_path <- file_path(main = main, sub = "raw", files = directory_dir, 
                             arg_checks = arg_checks)
  directory_pres <- file.exists(directory_path)

  downloads_yes <- c(!raw_data_pres, !directory_pres)
  if(!only_if_missing){
    downloads_yes <- rep(TRUE, ndl)
  }

  ndlyes <- sum(downloads_yes)
  if(ndlyes == 0){
    return(invisible(NULL))
  }
  messageq(" -Downloading raw files", quiet)
  dl_vers <- rep(NA, ndlyes)
  for(i in 1:ndlyes){
    yes_i <- which(downloads_yes)[i]
    downloads[[yes_i]]$cleanup <- ifnull(downloads[[yes_i]]$cleanup, 
                                     control_files$cleanup)
    downloads[[yes_i]]$main <- ifnull(downloads[[yes_i]]$main, main)
    downloads[[yes_i]]$quiet <- ifnull(downloads[[yes_i]]$quiet, quiet)
    downloads[[yes_i]]$sub <- "raw"
    dl_vers[i] <- do.call(download, downloads[[yes_i]])
  }
  update_directory_config(main = main, downloads_versions = dl_vers,
                          quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}

