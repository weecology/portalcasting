#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including (optionally) the raw data (\code{\link[portalr]{download_observations}}), portalPredictions archive (\code{\link{download_archive}}), climate forecasts (\code{\link{download_climate_forecasts}}), models (\code{\link{write_model}}).
#'             
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{list} of datasets to be created using \code{\link{do.call}} on the defined functions. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages).
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
#'   fill_raw("./pc")
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
fill_dir <- function (main     = ".",
                      models   = prefab_models(), 
                      datasets = prefab_rodent_datasets(),
                      settings = directory_settings(), 
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  messageq("Filling directory with requested content", quiet = quiet)

  fill_raw(main     = main, 
           settings = settings, 
           quiet    = quiet, 
           verbose  = verbose)

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


  fill_data(main = main, 
            datasets = datasets,
            quiet = quiet, 
            verbose = verbose,


models = models,
            end_moon = end_moon,
            start_moon = start_moon,
            lead_time = lead_time, min_lag = min_lag,
            confidence_level = confidence_level,
            cast_date = cast_date,
                      controls_model = controls_model,
                      control_files = control_files
)

  invisible()

}


#' @rdname fill_directory
#'
#' @export
#'

fill_data <- function (main  = ".",
 models = prefab_models(),
                        controls_model = NULL, 
                        control_files = files_control(), 
                        end_moon = NULL, start_moon = 217, lead_time = 12, 
                        min_lag = 6, 
                        confidence_level = 0.95, cast_date = Sys.Date(), 
                       datasets = prefab_rodent_datasets(),
                       quiet = FALSE,
                       verbose = TRUE) {



  messageq(" -Writing data files", quiet = quiet)

  data_r <- prepare_rodent_datasets(main = main,
                          datasets = datasets,
                          quiet = quiet,
                          verbose = verbose)


  messageq(" -Adding data files to data subdirectory", quiet = quiet)
  data_m <- prep_moons(main = main, lead_time = lead_time, 
                       cast_date = cast_date, 
                       quiet = quiet, verbose = verbose,
                       control_files = control_files)

  data_c <- prep_covariates(main = main, moons = data_m, end_moon = end_moon, 
                            start_moon = start_moon, lead_time = lead_time, 
                            min_lag = min_lag, cast_date = cast_date, 
                            quiet = quiet, control_files = control_files)

  prep_metadata(main = main, models = models,
                datasets = datasets, moons = data_m, 
                rodents = data_r, covariates = data_c, end_moon = end_moon, 
                lead_time = lead_time, min_lag = min_lag, 
                cast_date = cast_date, start_moon = start_moon, 
                confidence_level = confidence_level, 
                controls_model = controls_model,
                quiet = quiet, 
                control_files = control_files)

  invisible(NULL)
}

#' @rdname fill_directory
#'
#' @export
#'
fill_raw <- function (main     = ".",
                      settings = directory_settings(),
                      quiet    = FALSE,
                      verbose  = FALSE) {

  messageq("Downloading resources... \n ", quiet = quiet)

  download_observations(path        = file.path(main, "raw"), 
                        version     = settings$resources$PortalData$version,
                        from_zenodo = settings$resources$PortalData$source == "zenodo",
                        quiet       = quiet)

  download_archive(main = main, 
                   version = settings$resources$portalPredictions$version,
                   source = settings$resources$portalPredictions$source,
                   quiet = quiet,
                   verbose = verbose)

  download_climate_forecasts(main    = main, 
                             source  = settings$resources$climate_forecast$source,  
                             version = settings$resources$climate_forecast$version, 
                             data    = settings$resources$climate_forecast$data, 
                             quiet   = quiet,
                             verbose = verbose)

  messageq("\n  ... downloads complete.", quiet = quiet)
  
  invisible()

}

#' @rdname fill_directory
#'
#' @export
#'
fill_casts <- function (main = ".", quiet = FALSE, verbose = FALSE) {

  archive <- normalized_file_path(main, "raw", "portalPredictions", 
                                  mustWork = FALSE)

  casts <- normalized_file_path(main, "raw", "portalPredictions", 
                                "casts",  mustWork = FALSE)
  files <- list.files(casts, full.names = TRUE)

  if (length(files) == 0) {

    casts <- normalized_file_path(main, "raw", "portalPredictions", 
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

  archive <- normalized_file_path(main, "raw", "portalPredictions", 
                                  mustWork = FALSE)

  fits <- normalized_file_path(main, "raw", "portalPredictions", 
                                "fits",  mustWork = FALSE)
  files <- list.files(fits, full.names = TRUE)
  files[grepl("README", files)] <- NA
  files <- na.omit(files)

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

  invisible()

}


