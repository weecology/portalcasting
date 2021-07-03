#' @title Create the structure of a portalcasting directory
#'
#' @description Creates the necessary folder structure for a portalcasting 
#'              directory as well as a YAML file that tracks the setup 
#'              configurations. Folder paths are created internally 
#'              such that the user only needs to input the main folder's 
#'              location components (a la \code{\link[base]{file.path}}).
#'              The subdirectories are hardcoded as \code{"casts"},
#'              \code{"data"}, \code{"fits"}, \code{"models"}, 
#'              and \code{"resources"}.
#'
#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. Default value (\code{"."}) 
#'              puts the directory in the present location. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @examples
#'  \donttest{
#'
#'   create_dir("./portalcasting")
#'
#'  }
#'
#' @export
#'
create_dir <- function (main  = ".",  
                        quiet = FALSE) {

  subs <- c("casts", "fits", "models", "resources", "data")

  messageq("Creating portalcasting directory at\n ", main_path(main = main), 
           quiet = quiet)

  mapply(FUN          = dir.create,
         path         = sub_path(main = main, sub = subs),
         showWarnings = FALSE,
         recursive    = TRUE)

  write_config(main = main)

  invisible()

}

