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
#' @export
#'
dirtree <- function(base = ".", main = "", subs = subdirs()){
  if (length(base) > 1){
    stop("`base` can only be of length = 1")
  }
  if (length(main) > 1){
    stop("`main` can only be of length = 1")
  }
  if (!is.character(base)){
    stop("`base` is not a character")
  }
  if (!is.character(main)){
    stop("`main` is not a character")
  }
  if (!("subdirs" %in% class(subs))){
    stop("`subs` is not a subdirs list")
  }
  tree <- list("base" = base, "main" = main, "subs" = subs)
  class(tree) <- c("dirtree", "list")
  tree
}

#' @title Define the names of the subdirectories in a forecasting directory
#'
#' @description Produces a vector of subdirectory names that is of class
#'   \code{subdirs}, and which should generally contain the elements
#'   \code{"predictions"}, \code{"models"}, \code{"PortalData"}, 
#'   \code{"data"}, and \code{"tmp"}. It is generally not advised to change
#'   the subdirectory structure at this time.
#'
#' @param type \code{character} name for quick generation of subdirectory
#'   vector. Presently only defined for the default input 
#'   \code{"portalcasting"}.
#'
#' @param subs Either [1] names of additional subdirectories to add to the
#'   vector set up by \code{type} or [2] an optional way to input all 
#'   subdirectory names (requires setting \code{type = NULL}). 
#'
#' @return Class-\code{subdirs} vector of character elements of 
#'   subdirectory names.
#'
#' @export
#'
subdirs <- function(type = "portalcasting", subs = NULL){
  if (is.null(type) & is.null(subs)){
    stop("both `type` and `subs` are NULL")
  }
  if (!("portalcasting" %in% type) & is.null(subs)){
    stop("`type` is not recognized and `subs` is NULL")
  }
  if (!is.null(subs) && !is.character(subs)){
    stop("`subs` is not a character")
  }
  if (!is.null(type) &&type == "portalcasting"){
    subs <- c(subs, "predictions", "models", "PortalData", "data", "tmp")
    subs <- unique(subs)
  }
  class(subs) <- "subdirs"
  subs
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
#' @export
#'
base_path <- function(tree = dirtree()){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
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
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
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
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  base <- tree$base
  main <- tree$main
  subs <- tree$subs
  normalizePath(file.path(base, main, subs), mustWork = FALSE)
}

#' @rdname base_path
#'
#' @description \code{sub_path}: Return the path for a specific \code{subs} 
#'   folder in the directory. 
#'
#' @param specific_sub \code{character}-value name of the specific 
#'   subdirectory of interest.
#'
#' @return \code{sub_paths}: The normalized path of the \code{specific_sub}
#'   folder in the directory tree as a character value (see 
#'   \code{\link{normalizePath}}).
#'
#' @export
#'
sub_path <- function(tree = dirtree(), specific_sub){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (length(specific_sub) > 1){
    stop("`specific_sub` can only be of length = 1")
  }
  if (!(specific_sub %in% tree$subs)){
    stop("`specific_sub` not in `tree`")
  }
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main, specific_sub), mustWork = FALSE)
}

#' @title Determine the path for a model in the model sub directory
#'
#' @description Return the normalized path for a specific model.
#'
#' @param tree \code{dirtree}-class list.
#'
#' @param model \code{character} name of the specific model.
#'
#' @param extension \code{character} file extension.
#'
#' @return The normalized path of the specified model script (see 
#'   \code{\link{normalizePath}}).
#'
#' @export
#'
model_path <- function(tree = dirtree(), model = NULL, extension = ".R"){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (is.null(model)){
    stop("`model` needs to be specified")
  }
  if (!is.character(model)){
    stop("`model` is not a character")
  }
  if (!all(model %in% models())){
    stop("`model` not in models() list")
  }
  if (is.null(extension)){
    stop("`extension` needs to be specified")
  }
  if (!is.character(extension)){
    stop("`extension` is not a character")
  }
  if (length(extension) > 1){
    stop("`extension` can only be length 1")
  }
  lext <- nchar(extension)
  spot <- rep(NA, lext)
  for(i in 1:lext){
    spot[i] <- substr(extension, i, i) == "."
  }
  if (sum(spot) != 1){
    stop("`extension` is not an extension")
  }
  base <- tree$base
  main <- tree$main
  sub <- "models"
  mod <- paste0(model, extension)
  normalizePath(file.path(base, main, sub, mod), mustWork = FALSE)
}

#' @title Determine the path for a file in the forecasting directory
#'
#' @description Return the normalized path for a specific file within the
#'   directory. 
#'
#' @param tree \code{dirtree}-class list.
#'
#' @param local_path \code{character} file path within the \code{main} level 
#'   of the portalcasting directory.
#'
#' @return The normalized path of the specified file (see 
#'   \code{\link{normalizePath}}).
#'
#' @export
#'
file_path <- function(tree = dirtree(), local_path = NULL){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (is.null(local_path)){
    stop("`local_path` needs to be specified")
  }
  if (!is.character(local_path)){
    stop("`local_path` is not a character")
  }
  base <- tree$base
  main <- tree$main
  normalizePath(file.path(base, main, local_path), mustWork = FALSE)
}
