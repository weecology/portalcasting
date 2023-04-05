#' @title Fill a Portalcasting Directory with Basic Components
#'
#' @description Fill the directory with components including: 
#'   * Resources ([`fill_resources`]) 
#'     * raw data ([`download_observations`][portalr::download_observations])
#'     * directory archive ([`download_archive`])
#'     * climate forecasts ([`download_climate_forecasts`])
#'   * Output 
#'     * forecasts ([`fill_forecasts`])
#'     * model fits ([`fill_fits`])
#'   * Data ([`fill_data`])  
#'     * rodent datasets ([`prepare_rodents`])
#'     * temporal (lunar) data ([`prepare_newmoons`])
#'     * covariates ([`prepare_covariates`])
#'     * metadata ([`prepare_metadata`])
#'   * Models ([`fill_models`]) 
#'     * model controls ([`write_model_controls`])
#'     * model scripts (if needed) ([`write_model_scripts`])
#'   * Web Application ([`fill_app`]) 
#'     * transfers app files from package to main
#'     * renders ([`render`][rmarkdown::render]) and sources ([`source`][base::source]) files into HTML. \cr \cr
#'   Additionally, new models and datasets can be added to the directory at filling using the optional arguments `new_model_controls` and `new_dataset_controls`, but the model or dataset must still be listed in its respective main argument, as well.
#'             
#' @param main `character` value of the name of the main component of the directory tree.
#'
#' @param models `character` vector of name(s) of model(s) to include. Defaults to [`prefab_models`]. If controls are provided in `new_model_controls`, the model still needs to be named here to be included.
#'
#' @param datasets `character` vector of name(s) of rodent dataset(s) to be created. Defaults to [`prefab_datasets`]. If controls are provided in `new_dataset_controls`, the dataset still needs to be named here to be included.
#'
#' @param new_dataset_controls Optional named `list` of controls for new datasets. See [`dataset_controls`].
#'
#' @param new_model_controls Optional named `list` of controls for new models. See [`model_controls`].
#'
#' @return `NULL`, [`invisible`][base::invisible]-ly.
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
                      new_model_controls   = NULL) {

  settings <- read_directory_settings(main = main)

  messageq("Filling directory with content: \n", quiet = settings$quiet)

  fill_resources(main            = main)

  fill_forecasts(main            = main)

  fill_fits(main                 = main)

  fill_models(main               = main, 
              models             = models,
              new_model_controls = new_model_controls)

  fill_data(main                 = main, 
            datasets             = datasets,
            new_dataset_controls = new_dataset_controls)

  fill_app(main                  = main)

  messageq("\nDirectory filling complete.", quiet = settings$quiet)

  invisible( )

}


#' @rdname directory-filling
#'
#' @export
#'
fill_data <- function (main                 = ".",
                       datasets             = prefab_datasets( ),
                       new_dataset_controls = NULL) {

  settings <- read_directory_settings(main = main)


  messageq(" Preparing data files ... ", quiet = settings$quiet)
  messageq("  ... removing existing data files ... ", quiet = settings$quiet)

  unlink(x = list.files(path       = file.path(main, settings$subdirectories$data),
                        full.names = TRUE),
         force = TRUE)

  messageq("  ... adding data files ... ", quiet = settings$quiet)

  prepare_newmoons(main                = main)

  prepare_rodents(main                 = main,  
                  datasets             = datasets,
                  new_dataset_controls = new_dataset_controls) 

  prepare_covariates(main              = main)

  prepare_metadata(main                 = main, 
                   datasets             = datasets,
                   new_dataset_controls = new_dataset_controls)

  messageq(" ... complete.\n", quiet = settings$quiet)
  invisible( )

}


#' @rdname directory-filling
#'
#' @export
#'
fill_app <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  messageq("Setting up local web app instance ... ", quiet = settings$quiet)

  app_directory <- system.file(...     = "app", 
                               package = "portalcasting")

  file.copy(from      = list.files(app_directory, full.names = TRUE),
            to        = file.path(main, settings$subdirectories$app),
            recursive = TRUE,
            overwrite = TRUE)

  models_rmd <- file.path(main, settings$subdirectories$app, "models.Rmd")
  profiles_r <- file.path(main, settings$subdirectories$app, "profile_html.R")

  render(models_rmd, quiet = !settings$verbose)
  source(profiles_r, local = TRUE)

  messageq(" ... complete.\n", quiet = settings$quiet)
  invisible( )

}


#' @rdname directory-filling
#'
#' @export
#'
fill_resources <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  messageq("Downloading resources ... ", quiet = settings$quiet)

  download_observations(path      = file.path(main, settings$subdirectories$resources), 
                        version   = settings$resources$PortalData$version,
                        source    = settings$resources$PortalData$source,
                        pause     = settings$unzip_pause,
                        timeout   = settings$download_timeout,
                        force     = settings$force,
                        quiet     = settings$quiet,
                        verbose   = settings$verbose)

  download_archive(main          = main, 
                   resources_sub = settings$subdirectories$resources,
                   version       = settings$resources$portalPredictions$version,
                   source        = settings$resources$portalPredictions$source,
                   pause         = settings$unzip_pause,
                   timeout       = settings$download_timeout,
                   force         = settings$force,
                   quiet         = settings$quiet,
                   verbose       = settings$verbose)

  download_climate_forecasts(main          = main, 
                             resources_sub = settings$subdirectories$resources,
                             source        = settings$resources$climate_forecast$source,  
                             version       = settings$resources$climate_forecast$version, 
                             data          = settings$resources$climate_forecast$data, 
                             timeout       = settings$download_timeout,
                             force         = settings$force,
                             quiet         = settings$quiet,
                             verbose       = settings$verbose)

  messageq(" ... complete.\n", quiet = settings$quiet)
  
  update_directory_configuration(main = main)

  invisible( )

}

#' @rdname directory-filling
#'
#' @export
#'
fill_forecasts <- function (main = ".") { 

  settings <- read_directory_settings(main = main)

  files <- list.files(path       = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$forecasts),
                      full.names = TRUE)

  if (!settings$force) {

    existing_files <- list.files(path = file.path(main, settings$subdirectories$forecasts))
    files_short    <- list.files(path = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$forecasts))
    short_meta     <- files_short == settings$files$forecast_metadata
    files          <- unique(c(files[!(files_short %in% existing_files)],
                               files[short_meta]))
    
  }

  if (length(files) == 0) {

    return(invisible( ))

  }

  messageq(paste0(" Located ", length(files), " cast file(s) in resources to be moved to directory ..."), quiet = settings$quiet)
  messageq("  ... moving ... ", quiet = settings$quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subdirectories$forecasts), 
                      recursive = TRUE)

  messageq(paste0("  ... ", sum(copied), " files moved. "), quiet = settings$quiet)


  invisible( )


  ### --- patch to deal with refactor backwards compat --- ###
    update_forecasts_folder(main = main)
  ### --- end patch --- ###


}


#' @rdname directory-filling
#'
#' @export
#'
fill_fits <- function (main = ".") { 

  settings <- read_directory_settings(main = main)

  files <- list.files(path       = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$fits),
                      full.names = TRUE)

  if (!settings$force) {

    existing_files <- list.files(path = file.path(main, settings$subdirectories$fits))
    files_short    <- list.files(path = file.path(main, settings$subdirectories$resources, settings$repository, settings$subdirectories$fits))
    files          <- unique(c(files[!(files_short %in% existing_files)]))   

  }

  if (length(files) == 0) {

    return(invisible( ))

  }

  messageq(paste0(" Located ", length(files), " fit file(s) in resources to be moved to directory ..."), quiet = settings$quiet)
  messageq("  ... moving ... ", quiet = settings$quiet)

  copied <- file.copy(from      = files, 
                      to        = file.path(main, settings$subdirectories$fits), 
                      recursive = TRUE)

  messageq(paste0("  ... ", sum(copied), " files moved. "), quiet = settings$quiet)

  invisible( )

}



#' @rdname directory-filling
#'
#' @export
#'
fill_models <- function (main               = ".", 
                         models             = prefab_models( ), 
                         new_model_controls = NULL) {

  controls <- write_model_controls(main               = main, 
                                   models             = models,
                                   new_model_controls = new_model_controls) 

  write_model_scripts(main     = main, 
                      controls = controls)

  invisible( )

}


