#' @title Create the Structure of a Directory and Fill It with Content or Update an Existing Directory
#'
#' @description Instantiates the necessary folder structure for a directory, writes the setup configuration file, and fills the directory with content. 
#'
#' @param quiet `logic` indicator if progress messages should be quieted.
#'
#' @param main `character` value of the name of the main component of the directory tree. Default value (`"."`) roots the directory in the present location. 
#'
#' @param models `character` vector of name(s) of model(s) to include.
#'
#' @param datasets `character` vector of name(s) of rodent dataset(s) to be created. 
#'
#' @param new_dataset_controls Optional `list` of controls for new datasets. See [`dataset_controls`]. This argument is not available in `setup_production`.
#'
#' @param new_model_controls Optional `list` of controls for new models. See [`model_controls`]. This argument is not available in `setup_production`.
#'
#' @param settings `list` of controls for the directory, with defaults set in [`directory_settings`].
#'
#' @param verbose `logic` indicator of whether or not to produce all of the messages.
#'
#' @return The `list` of directory settings [`invisible`][base::invisible]-ly.
#'
#' @name directory creation
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
update_dir <- function (main                 = ".",
                        models               = prefab_models( ), 
                        datasets             = prefab_datasets( ),
                        new_dataset_controls = NULL,
                        new_model_controls   = NULL,
                        settings             = directory_settings( ), 
                        quiet                = FALSE, 
                        verbose              = FALSE) {

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


  fill_dir(main                 = main,
           models               = models, 
           datasets             = datasets,
           new_dataset_controls = new_dataset_controls,
           new_model_controls   = new_model_controls)

  read_directory_configuration(main = main)

  messageq(break_lines( ), "Directory successfully updated.\n", break_lines( ), quiet = quiet)


}



#' @rdname directory-creation
#'
#' @export
#'
setup_dir <- function (main                 = ".",
                       models               = prefab_models( ), 
                       datasets             = prefab_datasets( ),
                       new_dataset_controls = NULL,
                       new_model_controls   = NULL,
                       settings             = directory_settings( ), 
                       quiet                = FALSE, 
                       verbose              = FALSE) {

  core_package <- package_version_finder("setup_dir")

  messageq(break_lines( ), "This is ", core_package[["package"]], " v", core_package[["version"]], "\n", 
           break_line( ), "Establishing directory at\n  ", normalizePath(file.path(main = main), mustWork = FALSE), "\n  ",
           format(Sys.time(), "%x %T %Z"), "\n", break_lines( ), quiet = quiet)


  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet,
             verbose  = verbose)

  fill_dir(main                 = main,
           models               = models, 
           datasets             = datasets,
           new_dataset_controls = new_dataset_controls,
           new_model_controls   = new_model_controls)

  read_directory_configuration(main = main)

  messageq(break_lines( ), "Directory successfully instantiated.\n", break_lines( ), quiet = quiet)

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
setup_sandbox <- function (main                 = ".",
                           models               = prefab_models( ), 
                           datasets             = prefab_datasets( ),
                           new_dataset_controls = NULL,
                           new_model_controls   = NULL,
                           settings             = sandbox_settings( ), 
                           quiet                = FALSE, 
                           verbose              = FALSE) {

  setup_dir(main                 = main,
            models               = models, 
            datasets             = datasets,
            new_dataset_controls = new_dataset_controls,
            new_model_controls   = new_model_controls,
            settings             = settings,
            quiet                = quiet,
            verbose              = verbose)

  messageq(castle(), "Sandbox directory successfully set up at \n\n  ", normalizePath(file.path(main = main)), "\n\nHappy model building!", quiet = quiet)

}



#' @title Create, Update, and Read the Directory Configuration File
#' 
#' @description The directory configuration file is a special file within the directory setup and has its own set of functions. \cr \cr
#'              `write_directory_configuration` creates the YAML metadata configuration file. It is (and should only be) called from within [`setup_dir`], as it captures information about the compute environment used to instantiate the directory. \cr \cr
#'              `read_directory_configuration` reads the YAML config file into the R session. \cr \cr
#'              `read_directory_configuration` reads the YAML config file into the R session and pulls just the directory settings list in.
#'
#' @param quiet `logic` indicator if progress messages should be quieted.
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param verbose `logic` indicator of whether or not to print out all of the messages.
#'
#' @param settings `list` of controls for the directory, with defaults set in [`directory_settings`].
#'
#' @return `list` of directory configurations, [`invisible`][base::invisible]-ly.
#'
#' @name directory configuration file
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
  
  config <- tryCatch(
              read_yaml(file.path(main, "directory_configuration.yaml")),
              error = function(x){NA}, 
              warning = function(x){NA})
  
  if (length(config) == 1 && is.na(config)) {

    stop("Directory configuration file is corrupted or missing")

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
  settings$time$cast_date               <- as.Date(settings$time$cast_date)

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
  settings$resources$climate_forecast_version <- settings$resources$climate_forecast$version

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

