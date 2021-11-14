#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including (optionally) the resources data (\code{\link[portalr]{download_observations}}), portalPredictions archive (\code{\link{download_archive}}), climate forecasts (\code{\link{download_climate_forecasts}}), models (\code{\link{write_model}}).
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
#' @param lead_time \code{integer} (or integer \code{numeric}) value for the number of timesteps forward a cast will cover.
#'
#' @param origin \code{Date} forecast origin, typically today's date (set using \code{\link{Sys.Date}}).
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @name fill_directory
#'
NULL

#' @rdname fill_directory
#'
#' @export
#'
fill_dir <- function (main      = ".",
                      models    = prefab_models(), 
                      datasets  = prefab_rodent_datasets(),
                      lead_time = 12,
                      origin    = Sys.Date(), 
                      settings  = directory_settings(), 
                      quiet     = FALSE, 
                      verbose   = FALSE) {

  messageq("Filling directory with content: \n", quiet = quiet)

  fill_resources(main     = main, 
                 settings = settings, 
                 quiet    = quiet, 
                 verbose  = verbose)

  fill_output(main     = main, 
              quiet    = quiet, 
              verbose  = verbose)

  fill_models(main    = main, 
              models  = models, 
              quiet   = quiet, 
              verbose = verbose)

  fill_data(main      = main, 
            datasets  = datasets,
            models    = models, 
            lead_time = lead_time, 
            origin    = origin, 
            settings  = settings,
            quiet     = quiet, 
            verbose   = verbose)

  messageq("\nDirectory filling complete.", quiet = quiet)

  invisible()

}


#' @rdname fill_directory
#'
#' @export
#'
fill_data <- function (main      = ".",
                       models    = prefab_models(), 
                       datasets  = prefab_rodent_datasets(),
                       lead_time = 12,
                       origin    = Sys.Date(), 
                       settings  = directory_settings(), 
                       quiet     = FALSE, 
                       verbose   = FALSE) {

  messageq(" Writing data files ... ", quiet = quiet)

  prepare_rodents(main     = main,
                  datasets = datasets,
                  quiet    = quiet,
                  verbose  = verbose)

  prepare_moons(main      = main, 
                lead_time = lead_time, 
                origin    = origin, 
                settings  = settings,
                quiet     = quiet, 
                verbose   = verbose)

  prepare_covariates(main      = main, 
                     origin    = origin, 
                     settings  = settings,
                     quiet     = quiet, 
                     verbose   = verbose)

  prepare_metadata(main      = main, 
                   datasets  = datasets,
                   models    = models, 
                   lead_time = lead_time, 
                   origin    = origin, 
                   settings  = settings,
                   quiet     = quiet, 
                   verbose   = verbose)

  messageq("  ... data preparing complete.", quiet = quiet)

}

#' @rdname fill_directory
#'
#' @export
#'
fill_resources <- function (main     = ".",
                            settings = directory_settings(),
                            quiet    = FALSE,
                            verbose  = FALSE) {

  messageq(" Downloading resources ... ", quiet = quiet)

  download_observations(path        = file.path(main, "resources"), 
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

  messageq("  ... downloads complete. ", quiet = quiet)

}

#' @rdname fill_directory
#'
#' @export
#'
fill_output <- function (main    = ".", 
                         quiet   = FALSE, 
                         verbose = FALSE) { 

  output_folders <- c("casts", "fits", "output", "predictions")

  files <- unlist(mapply(FUN        = list.files,
                         path       = mapply(FUN = file.path, 
                                                   main, "resources", "portalPredictions", output_folders),
                         full.names = TRUE))

  if (length(files) == 0) {

    return(invisible())

  }

  messageq(paste0(" Located ", length(files), " output files ... \n  ... moving ..."), quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, "output"), 
                      recursive = TRUE)

  messageq(paste(ifelse(sum(copied) > 0, 
                   paste("  moved:", basename(files[copied]), collapse = "\n   "),
                   ""),
                 ifelse(sum(!copied) > 0, 
                   paste("  not moved:", basename(files[!copied]), collapse = "\n   "),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)

  messageq(paste0("  ... ", sum(copied), " files moved. "), quiet = quiet)

}




#' @rdname fill_directory
#'
#' @export
#'
fill_models <- function (main    = ".", 
                         models  = prefab_models(), 
                         quiet   = FALSE, 
                         verbose = FALSE) {

  return_if_null(models)

  messageq(" Writing model scripts ... ", quiet = quiet)
  

  mapply(FUN   = write_model,
         main  = main, 
         model = models, 
         quiet = !verbose)

  messageq("  ... done. ", quiet = quiet)

}


