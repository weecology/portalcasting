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
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
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
#' @name directory filling
#'
#'
#' @export
#'
fill_dir <- function (main     = ".",
                      models   = prefab_models(), 
                      datasets = prefab_rodent_datasets(),
                      settings = directory_settings(), 
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  messageq("Filling directory with content: \n", quiet = quiet)

  fill_raw(main     = main, 
           settings = settings, 
           quiet    = quiet, 
           verbose  = verbose)

  fill_casts(main     = main, 
             settings = settings, 
             quiet    = quiet, 
             verbose  = verbose)

  fill_fits(main     = main, 
            settings = settings, 
            quiet    = quiet, 
            verbose  = verbose)

  fill_data(main     = main, 
            datasets = datasets,
            models   = models,
            settings = settings,
            quiet    = quiet, 
            verbose  = verbose)

  fill_models(main     = main, 
              settings = settings, 
              models   = models, 
              quiet    = quiet, 
              verbose  = verbose)

  messageq("\nDirectory filling complete.", quiet = quiet)

  invisible()

}


#' @rdname directory-filling
#'
#' @export
#'
fill_data <- function (main     = ".",
                       models   = prefab_models(), 
                       datasets = prefab_rodent_datasets(),
                       settings = directory_settings(), 
                       quiet    = FALSE, 
                       verbose  = FALSE) {

  messageq(" Writing data files ... ", quiet = quiet)

  prep_rodents(main     = main,
               settings  = settings,
               datasets = datasets,
               quiet    = quiet,
               verbose  = verbose)

  prep_moons(main      = main,  
             settings  = settings,
             quiet     = quiet, 
             verbose   = verbose)

  prep_covariates(main      = main,  
                  settings  = settings,
                  quiet     = quiet, 
                  verbose   = verbose)

  prep_metadata(main      = main, 
                datasets  = datasets,
                models    = models, 
                settings  = settings,
                quiet     = quiet, 
                verbose   = verbose)

  messageq("  ... data preparing complete.", quiet = quiet)

}



#' @rdname directory-filling
#'
#' @export
#'
fill_raw <- function (main     = ".",
                      settings = directory_settings(),
                      quiet    = FALSE,
                      verbose  = FALSE) {

  messageq("Downloading resources ... ", quiet = quiet)

  download_observations(path        = file.path(main, settings$subs$resources), 
                        version     = settings$resources$PortalData$version,
                        from_zenodo = settings$resources$PortalData$source == "zenodo",
                        quiet       = quiet)

  download_archive(main          = main, 
                   resources_sub = settings$subs$resources,
                   version       = settings$resources$portalPredictions$version,
                   source        = settings$resources$portalPredictions$source,
                   pause         = settings$unzip_pause,
                   timeout       = settings$download_timeout,
                   quiet         = quiet,
                   verbose       = verbose)

  download_climate_forecasts(main          = main, 
                             resources_sub = settings$subs$resources,
                             source        = settings$resources$climate_forecast$source,  
                             version       = settings$resources$climate_forecast$version, 
                             data          = settings$resources$climate_forecast$data, 
                             timeout       = settings$download_timeout,
                             quiet         = quiet,
                             verbose       = verbose)

  messageq("  ... downloads complete. ", quiet = quiet)
  
  invisible()

}

#' @rdname directory-filling
#'
#' @export
#'
fill_casts <- function (main     = ".", 
                        settings = directory_settings(), 
                        quiet    = FALSE, 
                        verbose  = FALSE) { 

  output_folders <- c("casts")

  files <- unlist(mapply(FUN        = list.files,
                         path       = mapply(FUN = file.path, 
                                                   main, settings$subs["resources"], settings$repository, output_folders),
                         full.names = TRUE))

  if (length(files) == 0) {

    return(invisible())

  }

  messageq(paste0(" Located ", length(files), " cast files ... \n  ... moving ..."), quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subs["forecasts"]), 
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

  invisible()

}


#' @rdname directory-filling
#'
#' @export
#'
fill_fits <- function (main     = ".", 
                       settings = directory_settings(), 
                       quiet    = FALSE, 
                       verbose  = FALSE) { 

  output_folders <- c("fits")

  files <- unlist(mapply(FUN        = list.files,
                         path       = mapply(FUN = file.path, 
                                                   main, settings$subs["resources"], settings$repository, output_folders),
                         full.names = TRUE))

  if (length(files) == 0) {

    return(invisible())

  }

  messageq(paste0(" Located ", length(files), " fit files ... \n  ... moving ..."), quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subs["model fits"]), 
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

  invisible()

}



#' @rdname directory-filling
#'
#' @export
#'
fill_models <- function (main     = ".", 
                         models   = prefab_models(), 
                         settings = directory_settings(), 
                         quiet    = FALSE, 
                         verbose  = FALSE) {

  return_if_null(models)

  messageq(" Writing model scripts ... ", quiet = quiet)

  nmodels <- length(models)

  for (i in 1:nmodels) {

    write_model(main     = main, 
                model    = models[i], 
                settings = settings, 
                quiet    = quiet, 
                verbose  = verbose)

  }

  messageq("  ... done. ", quiet = quiet)

}


