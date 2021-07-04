#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including (optionally) 
#'              the raw data (\code{\link[portalr]{download_observations}}), 
#'              portalPredictions archive (\code{\link{download_archive}}), 
#'              climate forecasts (\code{\link{download_climate_forecasts}}), 
#'              models (\code{\link{write_model}}), 
#'              and model-ready data (\code{\link{prepare_rodent_data}}).
#'             
#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
#'
#' @param PortalData_source,PortalData_version \code{character} values for the
#'        source and version of the Portal Data to download. 
#'        \cr \cr 
#'        Default values retrieve the latest data from github.
#'        \cr \cr 
#'        See \code{\link[portalr]{download_observations}}.
#'
#' @param portalPredictions_source,portalPredictions_version \code{character} 
#'        values for the source and version of the archive to download.
#'        \cr \cr 
#'        Default values point to github, but \code{version = NULL} indicates
#'        no download.
#'        \cr \cr 
#'        See \code{\link{download_archive}}.
#'
#' @param climate_source,climate_version  \code{character} values for the
#'        source and version of the climate forecasts to download.
#'        \cr \cr 
#'        Default values retrieve the current day's forecast from the
#'        Northwest Knowledge Network's North American Multi-Model Ensemble 
#'        (NMME) climate forecasts.
#'        \cr \cr 
#'        See \code{\link{download_climate_forecasts}}.
#'
#' @param models \code{vector} of models to be written via 
#'        \code{\link{write_model}}.
#'
#' @param datasets \code{list} of datasets to be created using 
#'        \code{\link{do.call}} on the defined functions. 
#'        \cr \cr
#'        For example, \code{\link{prefab_datasets}} use 
#'        \code{\link{prepare_rodent_data}}.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
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
                      PortalData_source = "gitub",
                      PortalData_version = "latest",
                      portalPredictions_source = "github",
                      portalPredictions_version = NULL,
                      climate_forecast_source = "NMME",
                      climate_forecast_version = Sys.Date(),
                      models = prefab_models(),
                      datasets = prefab_datasets(),
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

  fill_data(main = main, 
            datasets = datasets,
            quiet = quiet, 
            verbose = verbose)

  invisible()

}


#' @rdname fill_directory
#'
#' @export
#'
fill_data <- function (main  = ".",
                       datasets = prefab_datasets(),
                       quiet = FALSE,
                       verbose = TRUE) {



  messageq(" -Writing data files", quiet = quiet)

  prepare_rodent_datasets(main = main,
                          datasets = datasets,
                          quiet = quiet,
                          verbose = verbose)


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


