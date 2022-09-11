#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including \enumerate{
#'   \item{Resources (\code{\link{fill_raw}}): \itemize{
#'     \item{raw data (\code{\link[portalr]{download_observations}})}
#'     \item{directory archive (\code{\link{download_archive}})}
#'     \item{climate forecasts (\code{\link{download_climate_forecasts}})}}}
#'   \item{Output: \itemize{
#'     \item{forecasts (\code{\link{fill_casts}})}
#'     \item{model fits (\code{\link{fill_fits}})}}}
#'   \item{Data (\code{\link{fill_data}}):  \itemize{
#'     \item{rodent datasets (\code{\link{prep_rodents}})}
#'     \item{temporal (lunar) data (\code{\link{prep_moons}})}
#'     \item{covariates (\code{\link{prep_covariates}})}
#'     \item{metadata (\code{\link{prep_metadata}})}}}
#'   \item{Models (\code{\link{fill_models}})}
#'  }
#'             
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{character} vector of name(s) of rodent dataset(s) to be created. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param multiprocess \code{character} (or \code{logical}) configuration for mulit-processing, can be any value from \code{unix}, \code{windows}, \code{TRUE}, \code{FALSE}. Default value is \code{FALSE}.
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
                      datasets = prefab_datasets(),
                      settings = directory_settings(), 
                      multiprocess  = FALSE,
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  messageq("Filling directory with content: \n", quiet = quiet)

  if (multiprocess == TRUE) {
    multiprocess <- .Platform$OS.type
  }

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
            multiprocess = multiprocess,
            quiet    = quiet, 
            verbose  = verbose)


  write_model_controls(main     = main, 
                       settings = settings, 
                       models   = models, 
                       quiet    = quiet)

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
#'library(portalcasting)
fill_data <- function (main         = ".",
                       models       = prefab_models(),
                       datasets     = prefab_datasets(),
                       settings     = directory_settings(), 
                       multiprocess = FALSE,
                       quiet        = FALSE,
                       verbose      = FALSE) {

  messageq(" Writing data files ... ", quiet = quiet)

  if (multiprocess == TRUE) {
    multiprocess <- .Platform$OS.type
  }

  write_data_set_controls_f <- function() {

    write_dataset_controls(main     = main, 
                         settings = settings, 
                         datasets = datasets,
                         multiprocess = multiprocess, 
                         quiet    = FALSE)

  }

  prep_rodents_f <- function() {

    prep_rodents(main     = main,
               settings = settings,
               datasets = datasets,
               quiet    = quiet,
               verbose  = verbose)

  }

  prep_moons_f <- function() {

    prep_moons(main      = main,  
             settings  = settings,
             quiet     = quiet, 
             verbose   = verbose)

  }

  prep_covariates_f <- function() {

    prep_covariates(main      = main,  
                  settings  = settings,
                  quiet     = quiet, 
                  verbose   = verbose)
    
  }

  prep_metadata_f <- function() {

    prep_metadata(main     = main, 
                datasets = datasets,
                models   = models, 
                settings = settings,
                quiet    = quiet, 
                verbose  = verbose)

  }

  if(multiprocess == 'unix') {

    write_data_set_controls_mc <- mcparallel(write_data_set_controls_f())
    prep_rodents_mc <-  mcparallel(prep_rodents_f())
    prep_moons_f_mc  <- mcparallel(prep_moons_f())
    prep_covariates_mc <- mcparallel(prep_covariates_f())
    prep_metadata_mc <- mcparallel(prep_metadata_f())
    mccollect(list(
      write_data_set_controls_mc,
      prep_rodents_mc,
      prep_moons_f_mc,
      prep_covariates_mc,
      prep_metadata_mc
    ))

  } else if (multiprocess == 'windows') {

    clusters <- makeCluster(detectCores() - 1, outfile = "")

    clusterExport(cl=clusters, varlist=c('main', 'datasets', 'models', 'settings', 'quiet', 'verbose', 'multiprocess'), envir=environment())

    parLapply(clusters, list(
      write_data_set_controls_f,
      prep_rodents_f,
      prep_moons_f,
      prep_covariates_f,
      prep_metadata_f
    ), function(prep_function) {
      prep_function()
    })

    stopCluster(clusters)

  } else {
    write_data_set_controls_f()
    prep_rodents_f()
    prep_moons_f()
    prep_covariates_f()
    prep_metadata_f()
  }

  messageq("  ... data preparing complete.", quiet = quiet)

  invisible()

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
  
  update_directory_config(main     = main,
                          settings = settings,
                          quiet    = quiet)

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
                                                   main, settings$subs$resources, settings$repository, output_folders),
                         full.names = TRUE))

  if (length(files) == 0) {

    return(invisible())

  }

  messageq(paste0(" Located ", length(files), " cast files ... \n  ... moving ..."), quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subs$forecasts), 
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
                                                   main, settings$subs$resources, settings$repository, output_folders),
                         full.names = TRUE))

  if (length(files) == 0) {

    return(invisible())

  }

  messageq(paste0(" Located ", length(files), " fit files ... \n  ... moving ..."), quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subs$fits), 
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

  model_controls_list <- model_controls(main     = main, 
                                        settings = settings, 
                                        models   = models)



  messageq(" Writing model scripts ... ", quiet = quiet)

  nmodels <- length(model_controls_list)

  for (i in 1:nmodels) {

    write_model(main     = main, 
                model    = models[i], 
                settings = settings, 
                quiet    = quiet, 
                verbose  = verbose)

  }

  messageq("  ... done. ", quiet = quiet)

  invisible()

}


