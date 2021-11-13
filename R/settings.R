#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param directory_config_file \code{character} value of the path to the directory config YAML.
#'
#' @return Named \code{list} of settings for the directory.
#'
#' @export
#'


directory_settings <- function(directory_config_file = "dir_config.yaml"){

  list(

    files = list(directory_config = directory_config_file),
    subs  = c("casts", "fits", "models", "raw", "data", "tmp")
  )

}

                        
