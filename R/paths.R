#' @title Determine a file's extension or remove the extension from the
#'  file path
#'
#' @description Based on the separating character, \code{file_ext} determines
#'  the file extension and \code{path_no_ext} determines the file path without
#'  the extension.
#'
#' @param path \code{character} value of the file path possibly with an
#'  extension.
#'
#' @param sep_char \code{character} value of the separator that delineates
#'  the extension from the file path. Generally, this will be \code{"."},
#'  but for some API URLs, the extension is actually a query component,
#'  so the separator may sometimes need to be \code{"="}.
#'
#' @return \code{character} value of the extension (\code{file_ext}) or the
#'  path without the extension (\code{path_no_ext}.
#' 
#' @examples
#'  file_ext("home/folders.with.dots/stuff/ok.csv")
#'  path_no_ext("home/folders.with.dots/stuff/ok.csv")
#'  file_ext(NMME_urls()[[1]])
#'  file_ext(NMME_urls()[[1]], "=")
#'
#' @export
#'
file_ext <- function(path, sep_char = "."){
  check_args()
  for_regexpr <- paste0("\\", sep_char, "([[:alnum:]]+)$")
  pos <- regexpr(for_regexpr, path)
  ifelse(pos > -1L, substring(path, pos + 1L), "")
}

#' @rdname file_ext
#'
#' @export
#'
path_no_ext <- function(path, sep_char = "."){
  check_args()
  for_sub <- paste0("([^", sep_char, "]+)\\.[[:alnum:]]+$")
  sub(for_sub, "\\1", path)
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
#'   subdirectory names (requires \code{subs_type = NULL}). 
#'
#' @param subs_type \code{character} name for quick generation of subdirectory
#'   vector. Presently only defined for \code{"prefab"}, or the setting
#'   \code{NULL} which allows for complete customization.
#'
#' @return \code{character} vector of subdirectory names.
#'
#' @examples
#'  subdirs()
#'
#' @export
#'
subdirs <- function(subs_names = NULL, subs_type = "prefab"){
  check_args()
  if (!is.null(subs_type)){
    if (subs_type == "prefab"){
      pc_subs <- c("predictions", "models", "raw", "data", "tmp")
      subs_names <- c(subs_names, pc_subs)
      subs_names <- unique(subs_names)
    } else{
      stop("subs_type can only be `prefab` or `NULL`")
    }
  }
  subs_names
}

#' @title Determine the path for a specific level of a forecasting directory
#'
#' @description Produce paths for a forecasting directory. \cr \cr
#'   \code{main_path} returns the path for the \code{main} folder. \cr \cr
#'   \code{sub_paths} returns the path for the \code{subs} folders. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param subs \code{character} vector of the names of the sub components of
#'  the directory tree. 
#'
#' @param specific_subs \code{character}-value name of the specific 
#'   subdirectory/subdirectories of interest, or \code{NULL} (default)
#'   for all.
#'
#' @return \code{character} value normalized paths 
#'   (see \code{\link{normalizePath}}) . \cr \cr
#'   \code{main_path} normalized path of the \code{main} folder. \cr \cr
#'   \code{subs_paths} normalized paths of the \code{subs} folders. \cr \cr
#' 
#' @examples
#'  \donttest{
#'   create_dir()
#'   main_path()
#'   sub_paths()
#'   sub_paths(specific_subs = "models")
#'  }
#'
#' @export
#'
main_path <- function(main = "."){
  check_args()
  fpath <- file.path(main)
  normalizePath(fpath, mustWork = FALSE)
}

#' @rdname main_path
#'
#' @export
#'
sub_paths <- function(main = ".", specific_subs = NULL, subs = subdirs()){
  check_args()
  if (!is.null(specific_subs) && (!all(specific_subs %in% subs))){
    stop("some `specific_subs` not in `subs`")
  }
  if (is.null(specific_subs)){
    specific_subs <- subs
  }
  fpath <- file.path(main, specific_subs)
  normalizePath(fpath, mustWork = FALSE)
}

#' @title Determine the path for one or more models in the model sub directory
#'
#' @description Return the normalized path(s) for a specific model or models.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param models \code{character} name of the specific model(s).
#'
#' @param extension \code{character} file extension (including the period).
#'
#' @return The normalized path of the specified model script (see 
#'   \code{\link{normalizePath}}) or \code{NULL} if \code{models = NULL}. 
#' 
#' @examples
#'  model_paths(models = "AutoArima")
#'
#' @export
#'
model_paths <- function(main = ".", models = NULL, extension = ".R"){
  check_args()
  return_if_null(models)
  sub <- "models"
  mod <- paste0(models, extension)
  fpath <- file.path(main, sub, mod)
  normalizePath(fpath, mustWork = FALSE)
}

#' @title Determine the path for a file or files in the forecasting directory
#'
#' @description Return the normalized path for a specific file or files 
#'   within the directory. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param local_paths \code{character} file path(s) within the \code{main}
#'   level of the portalcasting directory.
#'
#' @return The normalized path(s) of the specified file(s) (see 
#'   \code{\link{normalizePath}}) or \code{NULL} if \code{local_paths = NULL}. 
#' 
#' @examples
#'  file_paths(local_paths = "PortalData/Rodents/Portal_rodent_species.csv")
#'
#' @export
#'
file_paths <- function(main = ".", local_paths = NULL){
  return_if_null(local_paths)
  check_args()
  return_if_null(local_paths)
  fpath <- file.path(main, local_paths)
  normalizePath(fpath, mustWork = FALSE)
}
