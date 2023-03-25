#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including: \enumerate{
#'   \item{Resources (\code{\link{fill_resources}}) \itemize{
#'     \item{raw data (\code{\link[portalr]{download_observations}})}
#'     \item{directory archive (\code{\link{download_archive}})}
#'     \item{climate forecasts (\code{\link{download_climate_forecasts}})}}}
#'   \item{Output \itemize{
#'     \item{forecasts (\code{\link{fill_forecasts}})}
#'     \item{model fits (\code{\link{fill_fits}})}}}
#'   \item{Data (\code{\link{fill_data}})  \itemize{
#'     \item{rodent datasets (\code{\link{prepare_rodents}})}
#'     \item{temporal (lunar) data (\code{\link{prepare_newmoons}})}
#'     \item{covariates (\code{\link{prepare_covariates}})}
#'     \item{metadata (\code{\link{prepare_metadata}})}}}
#'   \item{Models (\code{\link{fill_models}}) \itemize{
#'     \item{model controls (\code{\link{write_model_controls}})}
#'     \item{model scripts (if needed) (\code{\link{write_model_scripts}})}}}
#'    }
#'    Additional, new models and datasets can be added to the directory at filling using the optional arguments \code{new_model_controls} and \code{new_dataset_controls}, but the model or dataset must still be listed in its respective main argument, as well.
#'             
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include. Defaults to \code{\link{prefab_models}}. If controls are provided in \code{new_model_controls}, the model still needs to be named here to be included.
#'
#' @param datasets \code{character} vector of name(s) of rodent dataset(s) to be created. Defaults to \code{\link{prefab_datasets}}. If controls are provided in \code{new_dataset_controls}, the dataset still needs to be named here to be included.
#'
#' @param new_dataset_controls Optional \code{list} of controls for new datasets. See \code{\link{dataset_controls}}.
#'
#' @param new_model_controls Optional \code{list} of controls for new models. See \code{\link{model_controls}}.
#'
#' @param settings \code{list} of controls for the directory. Defaults are set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @name directory filling
#'
NULL

#' @rdname directory-filling
#'
#' @export
#'
fill_dir <- function (main                 = ".",
                      models               = prefab_models( ), 
                      datasets             = prefab_datasets( ),
                      new_dataset_controls = NULL,
                      new_model_controls   = NULL,
                      settings             = directory_settings( ), 
                      quiet                = FALSE, 
                      verbose              = FALSE) {

  messageq("Filling directory with content: \n", quiet = quiet)

  fill_resources(main            = main, 
                 settings        = settings, 
                 quiet           = quiet, 
                 verbose         = verbose)

  fill_forecasts(main            = main, 
                 settings        = settings, 
                 quiet           = quiet, 
                 verbose         = verbose)

  fill_fits(main                 = main, 
            settings             = settings, 
            quiet                = quiet, 
            verbose              = verbose)

  fill_data(main                 = main, 
            datasets             = datasets,
            models               = models,
            new_dataset_controls = new_dataset_controls,
            settings             = settings,
            quiet                = quiet, 
            verbose              = verbose)

  fill_models(main               = main, 
              settings           = settings,
              models             = models,
              new_model_controls = new_model_controls,
              quiet              = quiet, 
              verbose            = verbose)

  messageq("\nDirectory filling complete.", quiet = quiet)

  invisible( )

}


#' @rdname directory-filling
#'
#' @export
#'
fill_data <- function (main                 = ".",
                       models               = prefab_models( ),
                       datasets             = prefab_datasets( ),
                       new_dataset_controls = NULL,
                       settings             = directory_settings( ), 
                       quiet                = FALSE,
                       verbose              = FALSE) {

  messageq(" Preparing data files ... ", quiet = quiet)
  messageq("  ... removing existing data files ... ", quiet = quiet)

  unlink(x = list.files(path       = file.path(main, settings$subdirectories$data),
                        full.names = TRUE),
         force = TRUE)

  messageq("  ... adding data files ... ", quiet = quiet)

  prepare_newmoons(main                = main,  
                   settings            = settings,
                   quiet               = quiet, 
                   verbose             = verbose)

  prepare_rodents(main                 = main,  
                  datasets             = datasets,
                  new_dataset_controls = new_dataset_controls,
                  settings             = settings,
                  quiet                = quiet, 
                  verbose              = verbose) 

  prepare_covariates(main              = main, 
                     settings          = settings,
                     quiet             = quiet, 
                     verbose           = verbose)

  prepare_metadata(main                 = main, 
                   datasets             = datasets,
                   new_dataset_controls = new_dataset_controls,
                   models               = models, 
                   settings             = settings,
                   quiet                = quiet, 
                   verbose              = verbose)

  messageq(" ... complete.\n", quiet = quiet)
  invisible( )

}



#' @rdname directory-filling
#'
#' @export
#'
fill_resources <- function (main     = ".",
                            settings = directory_settings( ),
                            quiet    = FALSE,
                            verbose  = FALSE) {

  messageq("Downloading resources ... ", quiet = quiet)

  download_observations(path      = file.path(main, settings$subdirectories$resources), 
                        version   = settings$resources$PortalData$version,
                        source    = settings$resources$PortalData$source,
                        pause     = settings$unzip_pause,
                        timeout   = settings$download_timeout,
                        overwrite = settings$overwrite,
                        quiet     = quiet,
                        verbose   = verbose)

  download_archive(main          = main, 
                   resources_sub = settings$subdirectories$resources,
                   version       = settings$resources$portalPredictions$version,
                   source        = settings$resources$portalPredictions$source,
                   pause         = settings$unzip_pause,
                   timeout       = settings$download_timeout,
                   overwrite     = settings$overwrite,
                   quiet         = quiet,
                   verbose       = verbose)

  download_climate_forecasts(main          = main, 
                             resources_sub = settings$subdirectories$resources,
                             source        = settings$resources$climate_forecast$source,  
                             version       = settings$resources$climate_forecast$version, 
                             data          = settings$resources$climate_forecast$data, 
                             timeout       = settings$download_timeout,
                             overwrite     = settings$overwrite,
                             quiet         = quiet,
                             verbose       = verbose)

  messageq(" ... complete.\n", quiet = quiet)
  
  update_directory_configuration(main     = main,
                                 settings = settings,
                                 quiet    = quiet)

  invisible( )

}

#' @rdname directory-filling
#'
#' @export
#'
fill_forecasts <- function (main     = ".", 
                            settings = directory_settings( ), 
                            quiet    = FALSE, 
                            verbose  = FALSE) { 

  files <- list.files(path       = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$forecasts),
                      full.names = TRUE)

  if (!settings$overwrite) {

    existing_files <- list.files(path = file.path(main, settings$subdirectories$forecasts))
    files_short    <- list.files(path = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$forecasts))
    short_meta     <- files_short == settings$files$forecast_metadata
    files          <- unique(c(files[!(files_short %in% existing_files)],
                               files[short_meta]))
    
  }

  if (length(files) == 0) {

    return(invisible( ))

  }

  messageq(paste0(" Located ", length(files), " cast file(s) in resources to be moved to directory ..."), quiet = quiet)
  messageq("  ... moving ... ", quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subdirectories$forecasts), 
                      recursive = TRUE)

  messageq(paste(ifelse(sum(copied) > 0, 
                   paste("  moved:", basename(files)[copied], collapse = "\n   "),
                   ""),
                 ifelse(sum(!copied) > 0, 
                   paste("  not moved:", basename(files)[!copied], collapse = "\n   "),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)


  # patch to deal with refactor backwards compat
    update_forecasts_folder(main = main, settings = settings, quiet = quiet)
  # end patch

  messageq(paste0("  ... ", sum(copied), " files moved. "), quiet = quiet)


  invisible( )

}


#' @rdname directory-filling
#'
#' @export
#'
fill_fits <- function (main     = ".", 
                       settings = directory_settings( ), 
                       quiet    = FALSE, 
                       verbose  = FALSE) { 

  files <- list.files(path       = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$fits),
                      full.names = TRUE)

  if (!settings$overwrite) {

    existing_files <- list.files(path = file.path(main, settings$subdirectories$fits))
    files_short    <- list.files(path = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$fits))
    files          <- unique(c(files[!(files_short %in% existing_files)]))   

  }

  if (length(files) == 0) {

    return(invisible( ))

  }

  messageq(paste0(" Located ", length(files), " fit file(s) in resources to be moved to directory ..."), quiet = quiet)
  messageq("  ... moving ... ", quiet = quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subdirectories$fits), 
                      recursive = TRUE)

  messageq(paste(ifelse(sum(copied) > 0, 
                   paste("  moved:", basename(files)[copied], collapse = "\n   "),
                   ""),
                 ifelse(sum(!copied) > 0, 
                   paste("  not moved:", basename(files)[!copied], collapse = "\n   "),
                   ""),
                 collapse = "\n"),
           quiet = !verbose)


  messageq(paste0("  ... ", sum(copied), " files moved. "), quiet = quiet)

  invisible( )

}



#' @rdname directory-filling
#'
#' @export
#'
fill_models <- function (main               = ".", 
                         models             = prefab_models( ), 
                         new_model_controls = NULL,
                         settings           = directory_settings( ), 
                         quiet              = FALSE, 
                         verbose            = FALSE) {

  model_controls_list <- write_model_controls(main               = main, 
                                              settings           = settings,
                                              models             = models,
                                              new_model_controls = new_model_controls,
                                              quiet              = quiet) 

  write_model_scripts(model_controls_list = model_controls_list,
                      quiet               = quiet,
                      verbose             = verbose)

  invisible( )

}


