#' @title Create the Structure of a Directory and Fill It with Content
#'
#' @description Instantiates the necessary folder structure for a directory, writes the setup configuration file, and fills the directory with content. 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. Default value (\code{"."}) roots the directory in the present location. 
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{character} vector of name(s) of rodent dataset(s) to be created. 
#'
#' @param new_dataset_controls Optional \code{list} of controls for new datasets. See \code{\link{dataset_controls}}. This argument is not available in \code{setup_production}.
#'
#' @param new_model_controls Optional \code{list} of controls for new models. See \code{\link{model_controls}}. This argument is not available in \code{setup_production}.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param verbose \code{logical} indicator of whether or not to produce all of the messages.
#'
#' @return The \code{list} of directory settings \code{\link[base]{invisible}}-ly.
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
                       quiet    = FALSE){

  core_package_version <- package_version_finder("setup_dir")

  messageq(break_line( ), "This is ", core_package_version[["package"]], " v", core_package_version[["version"]], "  ", format(Sys.time(), "%x %T %Z"), "\n", break_line( ), quiet = quiet)
  messageq(break_line( ), "Establishing directory at\n ", normalizePath(file.path(main = main), mustWork = FALSE), "\n", break_line( ), quiet = quiet)


  mapply(FUN          = dir.create, 
         path         = file.path(main, settings$subdirectories),
         recursive    = TRUE,
         showWarnings = FALSE)

  write_directory_configuration(main     = main, 
                                settings = settings, 
                                quiet    = quiet)

  messageq(break_line( ), "Directory successfully instantiated\n", break_line( ), quiet = quiet)

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

  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet)

  fill_dir(main                 = main,
           models               = models, 
           datasets             = datasets,
           new_dataset_controls = new_dataset_controls,
           new_model_controls   = new_model_controls,
           settings             = settings,
           quiet                = quiet,
           verbose              = verbose)

  read_directory_configuration(main     = main,
                               settings = settings,
                               quiet    = quiet)

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
#'              \code{write_directory_configuration} creates the YAML metadata configuration file. It is (and should only be) called from within \code{\link{setup_dir}}, as it captures information about the compute environment used to instantiate the directory. \cr \cr
#'              \code{read_directory_configuration} reads the YAML config file into the R session.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the messages.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{list} of directory configurations, \code{\link[base]{invisible}}-ly.
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
                                           quiet    = FALSE){

  core_package_version <- package_version_finder("write_directory_configuration")

  config <- list(setup = list(date                 = as.character(Sys.Date()),
                              R_version            = sessionInfo()$R.version,
                              core_package_name    = core_package_version[["package"]],
                              core_package_version = core_package_version[["version"]]),
                 tree  = list(main                 = main, 
                              subdirectories       = settings$subdirectories),
                 raw   = settings$raw)

  write_yaml(x    = config, 
             file = file.path(main, settings$files$directory_config))

  invisible(config)

}



#' @rdname directory-configuration-file
#'
#' @export
#'
read_directory_configuration<- function (main     = ".", 
                                         settings = directory_settings( ), 
                                         quiet    = FALSE){
  
  config <- tryCatch(
              read_yaml(file.path(main, settings$files$directory_config)),
              error = function(x){NA}, warning = function(x){NA})
  
  if (length(config) == 1 && is.na(config)) {

    stop("Directory configuration file is corrupted or missing")

  }

  invisible(config)

}



#' @rdname directory-configuration-file
#'
#' @export
#'
update_directory_configuration <- function (main     = ".", 
                                            settings = directory_settings( ), 
                                            quiet    = FALSE,
                                            verbose  = FALSE){
  
  config <- read_directory_configuration(main     = main, 
                                         settings = settings,
                                         quiet    = quiet)

  # fix this so it grabs the actual values when `latest`

  config$raw$PortalData_version       <- settings$resources$PortalData$version
  config$raw$archive_version          <- ifnull(settings$resources$portalPredictions$version, "")
  config$raw$climate_forecast_version <- settings$resources$climate_forecast$version

  if (config$raw$PortalData_version == "latest") {

    config$raw$PortalData_version <- scan(file  = file.path(main, settings$subdirectories$resources, "PortalData", "version.txt"),
                                          what  = "character", 
                                          quiet = !verbose)

  }

  write_yaml(x    = config, 
             file = file.path(main, settings$files$directory_configuration))

  invisible(config)

}


#' @title Find an Object's Host Package and Version Information
#'
#' @description Locate basic package information of an R object. If nothing is input, it operates on itself. \cr
#'   If the object is sourced through multiple packages, each package and its version are included.
#'
#' @param what An R object.
#'
#' @return \code{list} of the object, its class, the packages it is sourced from / through, and the versions of those packages.
#'
#' @export
#'
package_version_finder <- function (what) {

  if (missing(what)) {

    what <- "package_version_finder"

  }

  object_expr       <- parse(text         = what)
  object_eval       <- eval(expr          = object_expr)
  object_class      <- class(x            = object_eval)
  location_name     <- find(what          = what)
  packages_names    <- sapply(X           = location_name,
                              FUN         = gsub,
                              pattern     = "package\\:",
                              replacement = "")
  packages_versions <- sapply(X           = packages_names,
                              FUN         = packageDescription,
                              fields      = "Version")
  
  names(packages_versions) <- packages_names

  list(object   = what,
       class    = object_class,
       package  = packages_names,
       version  = packages_versions)

}

#' @title Functions for Message Generation 
#'
#' @description 
#'   \code{messageq}: Optionally generate a message based on a logical input. Uses a wrapper on \code{\link[base]{message}} that, given the input to \code{quiet}, generates the message(s) in \code{...} or not. \cr \cr
#'   \code{break_line}: Creates a horizontal line of characters for messages. \cr \cr
#'   \code{castle}: Creates a text drawing of a sandcastle of characters for messages. 
#'
#' @param ... zero or more objects that can be coerced to \code{character} and are concatenated with no separator added, or a single condition object. See \code{\link[base]{message}}.
#'
#' @param quiet \code{logical} indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If \code{NA}, messages will not be translated. See \code{\link[base]{message}} and \code{\link[base]{gettext}}.
#'
#' @param appendLF \code{logical} indicator if messages given as a \code{character} string should have a newline appended. See \code{\link[base]{message}}.
#'
#' @param char \code{character} value to repeated \code{reps} times to form the break line. 
#'
#' @param reps \code{integer}-conformable value for number of times \code{char} is replicated.
#' 
#'
#' @return 
#'   \code{messageq}: A message is given, and \code{NULL} returned, \code{\link[base]{invisible}}-ly. \cr
#'   \code{break_line}: The \code{character} of the line to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   \code{castle}: The \code{character} of the sandcastle to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'
#' @name messages
#'
NULL

messageq <- function (..., 
                      quiet    = FALSE, 
                      domain   = NULL, 
                      appendLF = TRUE) {

  if (!quiet) {

    message(...,
            domain   = domain,
            appendLF = appendLF)

  }

  invisible( )

}

#' @rdname messages
#'
#' @export
#'
break_line <- function(char = "-",
                       reps = 60){
  
  paste(rep(char, reps), collapse = "") 

}


#' @rdname messages
#'
#' @export
#'
castle <- function ( ) {

"
                                            
             /\\                         
            /  \\                          ____
           /|  |\\                        / -- )   
          /_|__|_\\                      (_--_/ 
          |      |                       / /              
 __    __ |      | __    __             / / 
[  ]__[  ].      .[  ]__[  ]           / /  
|__         ____         __|      ____/ /__ 
   |      .|    |.      |        / .-----  )
   |      |      |      |       / /     / / 
   |      |      |      |      / /     / /  
~~~~~~~~~~~~~~~~~~~~~~~~~~------------~~~~~~~~~~~~~~
"

}