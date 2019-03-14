#' @title Define the names of the directory tree for a forecasting directory
#'
#' @description A cross-platform, tidy way to pass the full tree structure 
#'   for a portalcasting directory through the codebase. Default setup (for
#'   using in Portal Predictions) is with the tree based in the working 
#'   directory and no additional "main" level (so the subdirectories are 
#'   located within the working directory).
#'
#' @param base \code{character} name of the base folder where the directory 
#'   should or does exist. Default \code{"."} (working directory).
#'
#' @param main \code{character} name of the main folder within the base that
#'   contains the subdirectories. Default \code{""} (no main level included). 
#'
#' @param subs \code{character} vector naming the specific subdirectories
#'   within the portalcasting directory tree. Default \code{subdirs()} sets
#'   the subdirectories as \code{"predictions"}, \code{"models"},
#'   \code{"PortalData"}, \code{"data"}, and \code{"tmp"}. It is generally
#'   not advised to change the subdirectories. 
#'
#' @return Class-\code{dirtree} named list (elements: \code{base}, 
#'   \code{main}, and \code{subs}) representing the directory tree.
#' 
#' @examples
#' \dontrun{
#' 
#' dirtree()
#' }
#'
#' @export
#'
dirtree <- function(base = ".", main = "", 
                    subs = subdirs(subs_type = "portalcasting")){
  check_argsX()
  list("base" = base, "main" = main, "subs" = subs) %>%
  classy(c("dirtree", "list"))
}

#' @title Define the names of the subdirectories in a forecasting directory
#'
#' @description Produces a vector of subdirectory names that is of class
#'   \code{subdirs}, and which should generally contain the elements
#'   \code{"predictions"}, \code{"models"}, \code{"PortalData"}, 
#'   \code{"data"}, and \code{"tmp"}. It is generally not advised to change
#'   the subdirectory structure at this time.
#'
#' @param subs_names Either [1] names of additional subdirectories to add to
#'   the vector set up by \code{type} or [2] an optional way to input all 
#'   subdirectory names (requires \code{type = NULL}, which is the default). 
#'
#' @param subs_type \code{character} name for quick generation of subdirectory
#'   vector. Presently only defined for \code{"portalcasting"}.
#'
#' @return Class-\code{subdirs} vector of character elements of 
#'   subdirectory names.
#'
#' @export
#'
subdirs <- function(subs_names = NULL, subs_type = NULL){
  check_argsX()
  if (!is.null(subs_type) && subs_type == "portalcasting"){
    pc_subs <- c("predictions", "models", "PortalData", "data", "tmp")
    subs_names <- c(subs_names, pc_subs)
    subs_names <- unique(subs_names)
  }
  classy(subs_names, "subdirs")
}

#' @title Determine the path for a specific level of a portalcasting directory
#'
#' @description \code{base_path}: Return the path for the \code{base} folder
#'   in the directory. 
#'
#' @param tree \code{dirtree}-class list. See \code{\link{dirtree}}.
#'
#' @return \code{base_path}: The normalized path of the main folder in the 
#'   directory tree as a character value (see \code{\link{normalizePath}}).
#' 
#' @examples
#' \dontrun{
#'
#' setup_dir()
#' base_path()
#' main_path()
#' sub_paths()
#' sub_path(specific_sub = "models")
#' }
#'
#' @export
#'
base_path <- function(tree = dirtree()){
  check_argsX()
  base <- tree$base
  normalizePath(file.path(base), mustWork = FALSE)
}

#' @rdname base_path
#'
#' @description \code{main_path}: Return the path for the \code{main} folder
#'   in the directory. 
#'
#' @return \code{main_path}: The normalized path of the main folder in the 
#'   directory tree as a character value (see \code{\link{normalizePath}}).
#'
#' @export
#'
main_path <- function(tree = dirtree()){
  check_argsX()
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main), mustWork = FALSE)
}

#' @rdname base_path
#'
#' @description \code{sub_paths}: Return the paths for the \code{subs} folders
#'   in the directory. 
#'
#' @return \code{sub_paths}: The normalized paths of the sub folders in the 
#'   directory tree as a character vector (see \code{\link{normalizePath}}).
#'
#' @export
#'
sub_paths <- function(tree = dirtree()){
  check_argsX()
  base <- tree$base
  main <- tree$main
  subs <- tree$subs
  normalizePath(file.path(base, main, subs), mustWork = FALSE)
}

#' @rdname base_path
#'
#' @description \code{sub_path}: Return the path for a specific \code{subs} 
#'   folder or folders in the directory. 
#'
#' @param specific_sub \code{character}-value name of the specific 
#'   subdirectory/subdirectories of interest.
#'
#' @return \code{sub_paths}: The normalized path(s) of the \code{specific_sub}
#'   folder(s) in the directory tree as \code{character} value(s) (see 
#'   \code{\link{normalizePath}}).
#'
#' @export
#'
sub_path <- function(tree = dirtree(), specific_sub = "tmp"){
  check_argsX()
  if (!is.null(specific_sub) && (!all(specific_sub %in% tree$subs))){
    stop("`specific_sub` not in `tree`")
  }
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main, specific_sub), mustWork = FALSE)
}

#' @title Determine the path for one or more models in the model sub directory
#'
#' @description Return the normalized path(s) for a specific model or models.
#'
#' @param tree \code{dirtree}-class list.
#'
#' @param models \code{character} name of the specific model(s).
#'
#' @param extension \code{character} file extension (including the period).
#'
#' @return The normalized path of the specified model script (see 
#'   \code{\link{normalizePath}}).
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' model_paths(models = "AutoArima")
#' }
#'
#' @export
#'
model_paths <- function(tree = dirtree(), models = NULL, extension = ".R"){
  check_argsX()
  base <- tree$base
  main <- tree$main
  sub <- "models"
  mod <- paste0(models, extension)
  normalizePath(file.path(base, main, sub, mod), mustWork = FALSE)
}

#' @title Determine the path for a file or files in the forecasting directory
#'
#' @description Return the normalized path for a specific file or files 
#'   within the directory. 
#'
#' @param tree \code{dirtree}-class list.
#'
#' @param local_paths \code{character} file path(s) within the \code{main}
#'   level of the portalcasting directory.
#'
#' @return The normalized path(s) of the specified file(s) (see 
#'   \code{\link{normalizePath}}).
#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' file_paths(local_paths = "PortalData/Rodents/Portal_rodent_species.csv")
#' }
#'
#' @export
#'
file_paths <- function(tree = dirtree(), local_paths = NULL){
  check_argsX()
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main, local_paths), mustWork = FALSE)
}
