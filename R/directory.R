#' @title Create the Structure of a Directory and Fill It with Content or Update an Existing Directory
#'
#' @description Instantiates the necessary folder structure for a directory, writes the setup configuration file, and fills the directory with content. \cr 
#'              Options for pre-defined setups include `setup_sandbox` for quick and flexible builds and `setup_production` for robust, rigid builds, as defined in [`directory_settings`].
#'
#' @param quiet `logical` indicator if progress messages should be quieted.
#'
#' @param main `character` value of the name of the main component of the directory tree. Default value (`"."`) roots the directory in the present location. 
#'
#' @param models `character` vector of name(s) of model(s) to include.
#'
#' @param datasets `character` vector of name(s) of rodent dataset(s) to be created. 
#'
#' @param new_datasets_controls Optional `list` of controls for new datasets. See [`datasets_controls`]. This argument is not available in `setup_production`.
#'
#' @param new_models_controls Optional `list` of controls for new models. See [`models_controls`]. This argument is not available in `setup_production`.
#'
#' @param settings `list` of controls for the directory, with defaults set in [`directory_settings`].
#'
#' @param verbose `logical` indicator of whether or not to produce all of the messages.
#'
#' @return The `list` of directory settings [`invisible`][base::invisible]-ly.
#'
#' @name directory creation
#'
#' @aliases create setup directory directory-setup directory-creation 
#'
#' @family orchestration
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "standard")
#'    main2 <- file.path(tempdir(), "sandbox")
#'    main3 <- file.path(tempdir(), "production")
#'
#'    setup_dir(main = main1)
#'    setup_sandbox(main = main2)
#'    setup_production(main = main3)
#'
#'    update_dir(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#'    unlink(main2, recursive = TRUE)
#'    unlink(main3, recursive = TRUE)
#' }
#'
NULL

#' @rdname directory-creation
#'
#' @export
#'
create_dir <- function(main     = ".", 
                       settings = directory_settings( ), 
                       quiet    = FALSE, 
                       verbose  = FALSE){

  out <- mapply(FUN          = dir.create, 
                path         = file.path(main, settings$subdirectories),
                recursive    = TRUE,
                showWarnings = FALSE)

  if (any(out)) {
    messageq(" Creating folders: \n", paste0("   ", names(out)[out], "\n"), quiet = quiet)
  }

  write_directory_configuration(main     = main, 
                                settings = settings, 
                                quiet    = quiet)

}



#' @rdname directory-creation
#'
#' @export
#'
update_dir <- function (main                  = ".",
                        models                = prefab_models( ), 
                        datasets              = prefab_datasets( ),
                        new_datasets_controls = NULL,
                        new_models_controls   = NULL,
                        settings              = directory_settings( ), 
                        quiet                 = FALSE, 
                        verbose               = FALSE) {

  core_package <- package_version_finder("setup_dir")

  messageq(break_lines( ), "This is ", core_package[["package"]], " v", core_package[["version"]], "\n", 
           break_line( ), "Updating directory at\n  ", normalizePath(file.path(main = main), mustWork = FALSE), "\n  ",
           format(Sys.time(), "%x %T %Z"), "\n", break_lines( ), quiet = quiet)

  out <- mapply(FUN          = dir.create, 
                path         = file.path(main, settings$subdirectories),
                recursive    = TRUE,
                showWarnings = FALSE)

  if (any(out)) {
    messageq(" Creating folders: \n", paste0("   ", names(out)[out], "\n"), quiet = quiet)
  }

  messageq("Updating directory configuration file ... \n ... done.\n", quiet = quiet)

  write_directory_configuration(main     = main, 
                                settings = settings, 
                                quiet    = quiet)

  fill_dir(main                  = main,
           models                = models, 
           datasets              = datasets,
           new_datasets_controls = new_datasets_controls,
           new_models_controls   = new_models_controls)

  messageq(break_lines( ), "Directory successfully updated.\n", break_lines( ), quiet = quiet)

  read_directory_configuration(main = main)

}



#' @rdname directory-creation
#'
#' @export
#'
setup_dir <- function (main                  = ".",
                       models                = prefab_models( ), 
                       datasets              = prefab_datasets( ),
                       new_datasets_controls = NULL,
                       new_models_controls   = NULL,
                       settings              = directory_settings( ), 
                       quiet                 = FALSE, 
                       verbose               = FALSE) {

  core_package <- package_version_finder("setup_dir")

  messageq(break_lines( ), "This is ", core_package[["package"]], " v", core_package[["version"]], "\n", 
           break_line( ), "Establishing directory at\n  ", normalizePath(file.path(main = main), mustWork = FALSE), "\n  ",
           format(Sys.time(), "%x %T %Z"), "\n", break_lines( ), quiet = quiet)

  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet,
             verbose  = verbose)

  fill_dir(main                  = main,
           models                = models, 
           datasets              = datasets,
           new_datasets_controls = new_datasets_controls,
           new_models_controls   = new_models_controls)

  messageq(break_lines( ), "Directory successfully instantiated.\n", break_lines( ), quiet = quiet)

  read_directory_configuration(main = main)

}


#' @rdname directory-creation
#'
#' @export
#'
setup_production <- function (main     = ".",
                              models   = prefab_models( ), 
                              datasets = prefab_datasets( ),
                              settings = production_settings( ), 
                              quiet    = FALSE, 
                              verbose  = TRUE) {

  setup_dir(main     = main,
            models   = models,
            datasets = datasets,
            settings = settings,
            quiet    = quiet,
            verbose  = verbose)

}



#' @rdname directory-creation
#'
#' @export
#'
setup_sandbox <- function (main                  = ".",
                           models                = prefab_models( ), 
                           datasets              = prefab_datasets( ),
                           new_datasets_controls = NULL,
                           new_models_controls   = NULL,
                           settings              = sandbox_settings( ), 
                           quiet                 = FALSE, 
                           verbose               = FALSE) {

  setup_dir(main                  = main,
            models                = models, 
            datasets              = datasets,
            new_datasets_controls = new_datasets_controls,
            new_models_controls   = new_models_controls,
            settings              = settings,
            quiet                 = quiet,
            verbose               = verbose)

  messageq(castle(), "Sandbox directory successfully set up at \n\n  ", normalizePath(file.path(main = main)), "\n\nHappy model building!", quiet = quiet)

  read_directory_configuration(main = main)

}



#' @title Create, Update, and Read the Directory Configuration File
#' 
#' @description The directory configuration file is a special file within the directory setup and has its own set of functions. \cr \cr
#'              `write_directory_configuration` creates the YAML metadata configuration file. It is (and should only be) called from within [`setup_dir`], as it captures information about the compute environment used to instantiate the directory. \cr \cr
#'              `read_directory_configuration` reads the YAML config file into the R session. \cr \cr
#'              `read_directory_settings` reads the YAML config file into the R session and pulls just the directory settings list in.
#'
#' @param quiet `logical` indicator if progress messages should be quieted.
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param verbose `logical` indicator of whether or not to print out all of the messages.
#'
#' @param settings `list` of controls for the directory, with defaults set in [`directory_settings`].
#'
#' @return `list` of directory configurations, [`invisible`][base::invisible]-ly.
#'
#' @name directory configuration file
#'
#' @aliases config configuration dir-config directory-config
#'
#' @family orchestration
#' @family read-write
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "standard")
#'    setup_dir(main = main1)
#'
#'    settings1 <- read_directory_settings(main = main1)
#'    config1   <- read_directory_configuration(main = main1)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL

#' @rdname directory-configuration-file
#'
#' @export
#'
write_directory_configuration <- function (main     = ".", 
                                           settings = directory_settings( ), 
                                           quiet    = FALSE,
                                           verbose  = FALSE){

  core_package_version <- package_version_finder("write_directory_configuration")

  config <- list(setup = list(date                 = as.character(Sys.Date()),
                              R_version            = sessionInfo()$R.version,
                              core_package_name    = core_package_version[["package"]],
                              core_package_version = core_package_version[["version"]]),
                 tree  = list(main                 = main, 
                              subdirectories       = settings$subdirectories))
  settings <- c(settings, 
                quiet    = quiet,
                verbose  = verbose)
  config <- update_list(list     = config, 
                        settings = settings)

  write_yaml(x    = config, 
             file = file.path(main, "directory_configuration.yaml"))

  invisible(config)

}



#' @rdname directory-configuration-file
#'
#' @export
#'
read_directory_configuration<- function (main = ".") {
  
  if (!file.exists(main)) {

    stop("Directory not found at '", main, "' -- run `create_dir`")

  }

  config <- tryCatch(
              read_yaml(file.path(main, "directory_configuration.yaml")),
              error = function(x){NA}, 
              warning = function(x){NA})
  
  if (length(config) == 1 && is.na(config)) {

    stop("Directory configuration file not found in '", main, "' -- run `create_dir`")

  }

  invisible(config)

}


#' @rdname directory-configuration-file
#'
#' @export
#'
read_directory_settings <- function (main = ".") {
  
  settings <- read_directory_configuration(main = main)$settings
 
  settings$time$timeseries_start        <- as.Date(settings$time$timeseries_start)
  settings$time$timeseries_start_lagged <- as.Date(settings$time$timeseries_start_lagged)
  settings$time$forecast_start          <- as.Date(settings$time$forecast_start)
  settings$time$forecast_end            <- as.Date(settings$time$forecast_end)
  settings$time$forecast_end_buffered   <- as.Date(settings$time$forecast_end_buffered)
  settings$time$origin                  <- as.Date(settings$time$origin)
  settings$time$forecast_date           <- as.Date(settings$time$forecast_date)

  settings
}



#' @rdname directory-configuration-file
#'
#' @export
#'
update_directory_configuration <- function (main = ".") {
  
  config   <- read_directory_configuration(main = main)
  settings <- config$settings

  # fix this so it grabs the actual values when `latest`

  settings$resources$PortalData_version       <- settings$resources$PortalData$version
  settings$resources$archive_version          <- ifnull(settings$resources$portalPredictions$version, "")
  settings$resources$climate_forecast_version <- settings$resources$climate_forecasts$version

  if (settings$resources$PortalData_version == "latest") {

    settings$resources$PortalData_version <- scan(file  = file.path(main, settings$subdirectories$resources, "PortalData", "version.txt"),
                                                  what  = "character", 
                                                  quiet = !settings$verbose)

  }

  config$settings <- settings

  write_yaml(x    = config, 
             file = file.path(main, "directory_configuration.yaml"))

  invisible(config)

}


#' @title Create a List of Full Directory Paths
#' 
#' @description Upon creation (or updating) of the directory, all the standard file and subdirectory paths are set based on [`directory_settings`]. \cr
#'              `paths` produces the full path `list`, whose contents can then also be accessed with specialized functions, see `Details`.
#'
#' @details Wrapper functions for specific subdirectories and files include:   
#'   * Files
#'     * `rodents_dataset_path`  
#'     * `climate_forecasts_paths`  
#'     * `forecasts_metadata_path`  
#'     * `forecasts_evaluations_path`  
#'     * `forecasts_results_path`  
#'     * `newmoons_path`  
#'     * `covariates_path`  
#'     * `metadata_path` 
#'     * `models_controls_path`  
#'     * `models_rmd_path` 
#'     * `rodents_profiles_html_path` 
#'     * `rodents_profiles_csv_path` 
#'   * Subdirectories
#'     * `app_path`  
#'     * `data_path`  
#'     * `forecasts_path`  
#'     * `fits_path`  
#'     * `models_path`  
#'     * `resources_path`
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param dataset `character` value of name of rodent dataset.
#'
#' @return `list` of directory paths or specific `character` paths.
#'
#' @name directory paths
#'
#' @aliases paths
#'
#' @family orchestration
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "standard")
#'    create_dir(main = main1)
#'
#'    paths(main = main1)
#'
#'    newmoons_path(main = main1)
#'
#'    rodents_datasets_paths(main = main1)
#'    rodents_dataset_path(main = main1)
#'    rodents_datasets_paths(main = main1)
#'
#'    covariates_path(main = main1)
#'    climate_forecasts_paths(main = main1)
#'
#'    metadata_path(main = main1)
#'
#'    forecasts_metadata_path(main = main1)
#'    forecasts_evaluations_path(main = main1)
#'    forecasts_results_path(main = main1)
#'
#'    models_controls_path(main = main1)
#'
#'    models_rmd_path(main = main1)
#'    models_html_path(main = main1)
#'    about_md_path(main = main1)
#'    rodents_profiles_html_path(main = main1)
#'    rodents_profiles_csv_path(main = main1)
#'
#'    app_path(main = main1)
#'    data_path(main = main1)
#'    forecasts_path(main = main1)
#'    fits_path(main = main1)
#'    models_path(main = main1)
#'    resources_path(main = main1)
#' }
#'
NULL

#' @rdname directory-paths
#'
#' @export
#'
paths <- function (main = ".") {

  settings <- read_directory_settings(main = main)

  subdirectories           <- as.list(file.path(main, settings$subdirectories))
  names(subdirectories)    <- settings$subdirectories

  climate_forecasts        <- file.path(main, settings$subdirectories$resources, "NMME",  paste0(settings$resources$climate_forecasts$data, ".csv"))
  names(climate_forecasts) <- names(settings$resources$climate_forecasts$data)

  if (file.exists(file.path(main, settings$subdirectories$data, settings$files$datasets_controls))) {
  rodent_datasets_controls <- read_datasets_controls(main = main)

  } else {

    rodent_datasets_controls <- prefab_datasets_controls( )

  }

  rodents_datasets         <- file.path(main, settings$subdirectories$data, paste0("rodents_", tolower(names(rodent_datasets_controls)), ".csv")) 
  names(rodents_datasets)  <- names(rodent_datasets_controls)


  files <- list(newmoons                  = file.path(main, settings$subdirectories$data, settings$files$newmoons),
                covariates                = file.path(main, settings$subdirectories$data, settings$files$covariates),
                metadata                  = file.path(main, settings$subdirectories$data, settings$files$metadata),
                rodents_datasets          = rodents_datasets,
                rodents_datasets_controls = file.path(main, settings$subdirectories$data, settings$files$datasets_controls),

                climate_forecasts         = climate_forecasts,

                forecasts_metadata        = file.path(main, settings$subdirectories$forecasts, settings$files$forecasts_metadata),
                forecasts_evaluations     = file.path(main, settings$subdirectories$forecasts, settings$files$forecasts_evaluations),
                forecasts_results         = file.path(main, settings$subdirectories$forecasts, settings$files$forecasts_results),

                models_controls           = file.path(main, settings$subdirectories$models, settings$files$models_controls), 

                directory_configuration   = file.path(main, "directory_configuration.yaml"), 

                models_rmd                = file.path(main, settings$subdirectories$app, settings$files$models_rmd), 
                about_md                  = file.path(main, settings$subdirectories$app, settings$files$about_md), 
                models_html               = file.path(main, settings$subdirectories$app, settings$files$models_html), 
                rodents_profiles_html     = file.path(main, settings$subdirectories$app, settings$files$rodents_profiles_html), 
                rodents_profiles_csv      = file.path(main, settings$subdirectories$app, "www", settings$files$rodents_profiles_csv))

  list(main           = main,
       subdirectories = subdirectories,
       files          = files)
 
}

#' @rdname directory-paths
#'
#' @export
#'
models_rmd_path <- function (main = ".") {

  paths(main = main)$files$models_rmd

}

#' @rdname directory-paths
#'
#' @export
#'
models_html_path <- function (main = ".") {

  paths(main = main)$files$models_html

}

#' @rdname directory-paths
#'
#' @export
#'
about_md_path <- function (main = ".") {

  paths(main = main)$files$about_md

}

#' @rdname directory-paths
#'
#' @export
#'
rodents_profiles_html_path <- function (main = ".") {

  paths(main = main)$files$rodents_profiles_html

}

#' @rdname directory-paths
#'
#' @export
#'
rodents_profiles_csv_path <- function (main = ".") {

  paths(main = main)$files$rodents_profiles_csv

}


#' @rdname directory-paths
#'
#' @export
#'
rodents_datasets_controls_path <- function (main = ".") {

  paths(main = main)$files$rodents_datasets_controls

}


#' @rdname directory-paths
#'
#' @export
#'
models_controls_path <- function (main = ".") {

  paths(main = main)$files$models_controls

}

#' @rdname directory-paths
#'
#' @export
#'
rodents_dataset_path <- function (main    = ".",
                                  dataset = "all") {

  paths(main = main)$files$rodents_datasets[dataset]

}

#' @rdname directory-paths
#'
#' @export
#'
climate_forecasts_paths <- function (main = ".") {

  paths(main = main)$files$climate_forecasts

}

#' @rdname directory-paths
#'
#' @export
#'
forecasts_metadata_path <- function (main = ".") {

  paths(main = main)$files$forecasts_metadata

}

#' @rdname directory-paths
#'
#' @export
#'
forecasts_evaluations_path <- function (main = ".") {

  paths(main = main)$files$forecasts_evaluations

}

#' @rdname directory-paths
#'
#' @export
#'
forecasts_results_path <- function (main = ".") {

  paths(main = main)$files$forecasts_results

}


#' @rdname directory-paths
#'
#' @export
#'
newmoons_path <- function (main = ".") {

  paths(main = main)$files$newmoons

}

#' @rdname directory-paths
#'
#' @export
#'
covariates_path <- function (main = ".") {

  paths(main = main)$files$covariates

}

#' @rdname directory-paths
#'
#' @export
#'
metadata_path <- function (main = ".") {

  paths(main = main)$files$metadata

}

#' @rdname directory-paths
#'
#' @export
#'
app_path <- function (main = ".") {

  paths(main = main)$subdirectories$app
  
}

#' @rdname directory-paths
#'
#' @export
#'
data_path <- function (main = ".") {

  paths(main = main)$subdirectories$data
  
}

#' @rdname directory-paths
#'
#' @export
#'
forecasts_path <- function (main = ".") {

  paths(main = main)$subdirectories$forecasts
  
}

#' @rdname directory-paths
#'
#' @export
#'
fits_path <- function (main = ".") {

  paths(main = main)$subdirectories$fits
  
}

#' @rdname directory-paths
#'
#' @export
#'
models_path <- function (main = ".") {

  paths(main = main)$subdirectories$models
  
}

#' @rdname directory-paths
#'
#' @export
#'
resources_path <- function (main = ".") {

  paths(main = main)$subdirectories$resources
  
}

