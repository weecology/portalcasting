#' @title Create the Structure of a Forecasting Directory
#'
#' @description Instantiates the necessary folder structure for a forecasting directory and writes a YAML file that tracks the setup configurations. 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. Default value (\code{"."}) puts the forecasting directory in the present locations. Nesting the forecasting directory in a folder can be done by simply adding to the \code{main} input (see \code{Examples}).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return The \code{list} of directory settings \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   create_dir(main = "./main_folder")
#'  }
#'
#' @rdname directory_creation
#'
#' @export
#'
create_dir <- function(main     = ".", 
                       settings = directory_settings(), 
                       quiet    = FALSE){

  creation_message(main = main, quiet = quiet)

  mapply(FUN          = dir.create, 
         path         = file.path(main, settings$subs),
         recursive    = TRUE,
         showWarnings = FALSE)

  invisible(write_directory_config(main = main, settings = settings, quiet = quiet))
  
}


#' @title Create, Update, and Read the Directory Configuration File
#' 
#' @description The directory configuration file is a special file within the portalcasting directory setup and has its own set of functions. \cr \cr
#'              \code{write_directory_config} creates the YAML metadata configuration file. It is (and should only be) called from within \code{\link{create_dir}}, as it captures information about the compute environment used to create the directory. \cr \cr
#'              \code{update_directory_config} adds key components to the file; currently only adding the versions of the downloaded resources from within \code{\link{fill_raw}} (presently the only place it is and should be called from). \cr \cr
#'              \code{read_directory_config} reads the YAML config file into the R session.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. Default value (\code{"."}) puts the forecasting directory in the present locations. Nesting the forecasting directory in a folder can be done by simply adding to the \code{main} input (see \code{Examples}).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{list} of directory configurations. 
#'
#' @name directory_config
#'
NULL

#' @rdname directory_config
#'
#' @export
#'
write_directory_config <- function (main     = ".", 
                                    settings = directory_settings(), 
                                    quiet    = FALSE){

  config <- list(

              setup      = list(
                             date                  = as.character(Sys.Date()),
                             R_version             = sessionInfo()$R.version,
                             portalcasting_version = packageDescription("portalcasting", fields = "Version")),

              tree       = list(
                             main = main, 
                             subs = settings$subs),
 
              resources  = NULL

            ) 

  write_yaml(config, file = file.path(main, settings$files$directory_config))
  config

}

#' @rdname directory_config
#'
#' @export
#'
update_directory_config <- function (main      = ".", 
                                     settings  = directory_settings(), 
                                     quiet     = FALSE){
  
  config <- read_directory_config(main = main, 
                                  settings = settings,
                                  quiet = quiet)


  if (!is.null(settings$resources)) {

    config$resources <- settings$resources

  }

  write_yaml(config, file = file.path(main, settings$files$directory_config))
  config

}



#' @rdname directory_config
#'
#' @export
#'
read_directory_config <- function (main     = ".", 
                                   settings = directory_settings(), 
                                   quiet    = FALSE){
  
  config <- tryCatch(
              read_yaml(file.path(main, settings$files$directory_config)),
              error = function(x){NA}, warning = function(x){NA})
  
  if (length(config) == 1 && is.na(config)) {

    stop("directory configuration file is corrupted or missing", call. = FALSE)

  }

  config

}

