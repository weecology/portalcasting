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
#' @return \code{NULL} \code{\link[base]{invisible}}-ly.
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


  invisible() 
}

