#' @title Define the names of the directory tree for a forecasting directory
#'
#' @description A tidy way to pass the full tree structure down. 
#'
#' @param base name of the base directory where the directory should exist 
#'
#' @param main name of the forecasting directory
#'
#' @param subs names of the subdirectories 
#'
#' @return named list of representing the directory tree
#'
#' @export
#'
dirtree <- function(base = ".", main = "", subs = subdirs()){
  list("base" = base, "main" = main, "subs" = subs)
}

#' @title Define the names of the subdirectories in a forecasting directory
#'
#' @description Convenience function for producing a vector of subdirectory
#'   names
#'
#' @param type name for quick generation of subdirectory names
#'
#' @param subs names of additional subdirectories to add
#'
#' @return character vectory of subdirectory names
#'
#' @export
#'
subdirs <- function(type = "portalcasting", subs = NULL){
  if (type == "portalcasting"){
    subs <- c(NULL, "predictions", "models", "PortalData", "data", "tmp")
  }
  subs
}

#' @title Determine the path for the main directory
#'
#' @description Return the path for the main directory
#'
#' @param tree the name tree of the forecasting directory
#'
#' @return the path of the main folder in the directory tree
#'
#' @export
#'
main_path <- function(tree = dirtree()){
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main), mustWork = FALSE)
}

#' @title Determine the paths for the sub directories
#'
#' @description Return the paths for the sub directories
#'
#' @param tree the name tree of the forecasting directory
#'
#' @return the paths of the sub folders in the directory tree
#'
#' @export
#'
sub_paths <- function(tree = dirtree()){
  base <- tree$base
  main <- tree$main
  subs <- tree$subs
  normalizePath(file.path(base, main, subs), mustWork = FALSE)
}

#' @title Determine the path for a specific sub directory
#'
#' @description Return the paths for the sub directories
#'
#' @param tree the name tree of the forecasting directory
#'
#' @param specific_sub the name of the specific subdirectory of interest
#'
#' @return the paths of the sub folders in the directory tree
#'
#' @export
#'
sub_path <- function(tree = dirtree(), specific_sub){
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main, specific_sub), mustWork = FALSE)
}

#' @title Determine the path for a model in the model sub directory
#'
#' @description Return the path for a specific model
#'
#' @param tree the name tree of the forecasting directory
#'
#' @param model the name of the specific model
#'
#' @param extension file extension
#'
#' @return the path of specific model script
#'
#' @export
#'
model_path <- function(tree = dirtree(), model = NULL, extension = ".R"){
  base <- tree$base
  main <- tree$main
  sub <- "models"
  mod <- paste0(model, extension)
  normalizePath(file.path(base, main, sub, mod), mustWork = FALSE)
}

#' @title Determine the path for a file in the forecasting directory
#'
#' @description Return the path for a specific file
#'
#' @param tree the name tree of the forecasting directory
#'
#' @param local_path the file path within the forecasting directory
#'
#' @return the full path of the file
#'
#' @export
#'
file_path <- function(tree = dirtree(), local_path = NULL){
  if (is.null(local_path)){
    stop('"local_path" needs to be specified')
  }
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main, local_path), mustWork = FALSE)
}
