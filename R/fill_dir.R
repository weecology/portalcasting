#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including (optionally) 
#'              the raw data (\code{\link[portalr]{download_observations}}), 
#'              portalPredictions archive (\code{\link{download_archive}}), 
#'              climate forecasts (\code{\link{download_climate_forecasts}}), 
#'              models (\code{\link{write_model}}), 
#'              and model-ready data.
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
#'   create_dir("./pc")
#'   fill_resources("./pc")
#'   fill_casts("./pc")
#'   fill_fits("./pc")
#'   fill_models("./pc")
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
                      climate_forecast_version = Sys.Date(),
                      climate_forecast_source = "NMME",
                      models = prefab_models(),
                      quiet = FALSE,
                      verbose = TRUE) {

  messageq("Filling directory with requested content",
           quiet = quiet)

  fill_resources(main = main, 
                 PortalData_version = PortalData_version,
                 PortalData_source = PortalData_source,
                 portalPredictions_version = portalPredictions_version,
                 portalPredictions_source = portalPredictions_source,
                 quiet = quiet, verbose = verbose)

  fill_casts(main = main, 
             quiet = quiet, 
             verbose = verbose)

  fill_fits(main = main, 
            quiet = quiet, 
            verbose = verbose)

  fill_models(main = main, 
              models = models, 
              quiet = quiet, 
              verbose = verbose)


return()

#
# Working here. 
#


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
fill_resources <- function (main  = ".",
                            PortalData_version = "latest",
                            PortalData_source = "gitub",
                            portalPredictions_version = NULL,
                            portalPredictions_source = "github",
                            climate_forecast_version = Sys.Date(),
                            climate_forecast_source = "NMME",
                            quiet = FALSE,
                            verbose = FALSE) {

  messageq("Downloading resources...",
           quiet = quiet)

  download_observations(path = resources_path(main = main), 
                        version = PortalData_version,
                        from_zenodo = PortalData_source == "zenodo",
                        quiet = quiet,
                        verbose = verbose)

  download_archive(main = main, 
                   version = portalPredictions_version,
                   source = portalPredictions_source,
                   quiet = quiet,
                   verbose = verbose)

  download_climate_forecasts(main = main, 
                             source = climate_forecast_source,  
                             version = climate_forecast_version, 
                             quiet = quiet,
                             verbose = verbose)

  messageq("... downloads complete.",
           quiet = quiet)
  
  invisible()

}

#' @rdname fill_directory
#'
#' @export
#'
fill_casts <- function (main = ".", quiet = FALSE, verbose = FALSE) {

  archive <- normalized_file_path(main, "resources", "portalPredictions", 
                                  mustWork = FALSE)

  casts <- normalized_file_path(main, "resources", "portalPredictions", 
                                "casts",  mustWork = FALSE)
  files <- list.files(casts, full.names = TRUE)

  if (length(files) == 0) {

    casts <- normalized_file_path(main, "resources", "portalPredictions", 
                                  "predictions",  mustWork = FALSE)
    files <- list.files(casts, full.names = TRUE)    

  }  

  if (length(files) == 0) {

    return(invisible())

  }

  dest <- normalized_file_path(main, "casts",  mustWork = FALSE)
 
  fc <- file.copy(from = files, to = dest, recursive = TRUE)

  messageq(paste0(sum(fc), " of ", length(fc), " cast files moved"),
           quiet = quiet)

  messageq(paste(ifelse(sum(fc) > 0, 
                   paste("moved:", basename(files[fc]), collapse = "\n"),
                   ""),
                 ifelse(sum(!fc) > 0, 
                   paste("not moved:", basename(files[!fc]), collapse = "\n"),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)

  invisible()

}


#' @rdname fill_directory
#'
#' @export
#'
fill_fits <- function (main = ".", quiet = FALSE, verbose = FALSE) {

  archive <- normalized_file_path(main, "resources", "portalPredictions", 
                                  mustWork = FALSE)

  fits <- normalized_file_path(main, "resources", "portalPredictions", 
                                "fits",  mustWork = FALSE)
  files <- list.files(fits, full.names = TRUE)

  if (length(files) == 0) {

    return(invisible())

  }

  dest <- normalized_file_path(main, "fits",  mustWork = FALSE)
 
  fc <- file.copy(from = files, to = dest, recursive = TRUE)

  messageq(paste0(sum(fc), " of ", length(fc), " fit files moved"),
           quiet = quiet)

  messageq(paste(ifelse(sum(fc) > 0, 
                   paste("moved:", basename(files[fc]), collapse = "\n"),
                   ""),
                 ifelse(sum(!fc) > 0, 
                   paste("not moved:", basename(files[!fc]), collapse = "\n"),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)

  invisible()

}



#' @rdname fill_directory
#'
#' @export
#'
fill_models <- function (main = ".", 
                         models = prefab_models(), 
                         quiet = FALSE, 
                         verbose = FALSE) {


  return_if_null(models)

  messageq(" -Writing model scripts", quiet = quiet)
  nmodels <- length(models)

  for (i in 1:nmodels) {

    write_model(main = main, 
                model = models[i],
                quiet = !verbose)

  } 

  invisible(NULL)

}


