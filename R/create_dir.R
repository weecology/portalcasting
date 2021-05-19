#' @title Create the structure of a portalcasting directory
#'
#' @description Creates the necessary folder structure for a portalcasting 
#'              directory as well as a YAML file that tracks the setup 
#'              configurations. Folder paths are created internally using  
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
         path         = sub_path(main = main, subs = subs),
         showWarnings = FALSE,
         recursive    = TRUE)


  write_config(main = main)

  invisible()

}




#' @title Verify that folders exist 
#'
#' @description 
#'  throws an error if any of the folders (specified by 
#'  \code{path} and named by \code{level}) do not exist. \cr \cr
#'  
#' @param paths \code{character} vector of the folder paths.
#'
#' @return 
#'  throws an error if any of the folders do not exist, 
#'  otherwise \code{NULL}.
#'
#' @examples
#'   verify(".")
#'
#' @export
#'
verify <- function(paths = NULL){
  return_if_null(paths)
  misses <- NULL
  for(i in 1:length(paths)){
    if (!dir.exists(paths[i])){
      misses <- c(misses, paths[i])
    }
  }
  nmisses <- length(misses)
  if (nmisses == 0){
    return(invisible(NULL))
  } 
  if (nmisses == 1){
    msg <- paste0("\n Folder does not exist at ", misses)
  } else{
    msg_m <- paste(misses, collapse = ", ")
    msg <- paste0("\n Folders do not exist at ", msg_m)
  }
  msg2 <- c("Missing directory components", msg, 
             "\n Run `create_dir` to create directory")
  stop(msg2, call. = FALSE)
}
