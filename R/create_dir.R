#' @title Create the Structure of a Forecasting Directory
#'
#' @description Instantiates the necessary folder structure for a forecasting directory and writes a YAML file that tracks the setup configurations. 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. Default value (\code{"."}) puts the forecasting directory in the present locations. Nesting the forecasting directory in a folder can be done by simply adding to the \code{main} input (see \code{Examples}).
#'
#' @param arg_checks \code{logical} value of if the arguments should be checked using standard protocols via \code{\link{check_args}}. The default (\code{arg_checks = TRUE}) ensures that all inputs are formatted correctly and provides directed error messages if not.
#'
#' @param filename_config \code{character} value of the path to the directory
#'  config YAML.
#'
#' @return \code{NULL} \code{\link[base]{invisible}}-ly.
#'
#' @rdname directory_creation
#'
#' @export
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'  }
#'
create_dir <- function(main            = ".", 
                       quiet           = FALSE, 
                       filename_config = "dir_config.yaml",
                       arg_checks      = TRUE){

  check_args(arg_checks = arg_checks)
  messageq(message_break(), "\nEstablishing portalcasting directory at\n ", normalizePath(file.path(main = main), mustWork = FALSE), "\n", message_break(), quiet = quiet)

  subs <- c("casts", "fits", "models", "raw", "data", "tmp")

  mapply(FUN          = dir.create, 
         path         = file.path(main, subs),
         recursive    = TRUE,
         showWarnings = FALSE)

  write_directory_config(main            = main, 
                         filename_config = filename_config, 
                         quiet           = quiet, 
                         arg_checks      = arg_checks)
  invisible() 
}