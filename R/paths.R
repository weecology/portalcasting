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
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
file_ext <- function(path, sep_char = ".", arg_checks = TRUE){
  check_args(arg_checks)
  for_regexpr <- paste0("\\", sep_char, "([[:alnum:]]+)$")
  pos <- regexpr(for_regexpr, path)
  ifelse(pos > -1L, substring(path, pos + 1L), "")
}

#' @rdname file_ext
#'
#' @export
#'
path_no_ext <- function(path, sep_char = ".", arg_checks = TRUE){
  check_args(arg_checks)
  for_sub <- paste0("([^", sep_char, "]+)\\.[[:alnum:]]+$")
  sub(for_sub, "\\1", path)
}


#' @title Determine the path for a specific level of a forecasting directory
#'
#' @description Produce paths for a forecasting directory. \cr \cr
#'  \code{main_path} returns the path for the \code{main} folder. \cr \cr
#'  \code{sub_path} returns the path(s) for the \code{sub} folder(s). \cr \cr
#'  \code{file_path} returns the path(s) for the \code{files}. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param sub,subs \code{character} of the name(s) of the sub component(s) of
#'  the directory tree to get the path of. \code{sub} can only take a single
#'  value, \code{subs} can take a vector of multiple values.
#'
#' @param files \code{character} file path(s) within the \code{main}
#'   level of the portalcasting directory.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not.
#'
#' @return \code{character} value normalized paths 
#'  (see \code{\link{normalizePath}}) . \cr \cr
#'  \code{main_path} normalized path of the \code{main} folder. \cr \cr
#'  \code{subs_path} normalized paths of the \code{sub} folder(s). \cr \cr
#'  \code{file_path} normalized paths of the \code{files}. 
#' 
#' @examples
#'  \donttest{
#'   create_dir()
#'   main_path()
#'   sub_path()
#'   sub_path(subs = "models")
#'   file_path(".", "raw", "PortalData/Rodents/Portal_rodent_species.csv")
#'  }
#'
#' @name paths
#'
NULL

#' @rdname paths
#'
#' @export
#'
main_path <- function(main = ".", arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  fpath <- file.path(main)
  normalizePath(fpath, mustWork = FALSE)
}

#' @rdname paths
#'
#' @export
#'
sub_path <- function(main = ".", subs = NULL, arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(subs)
  fpath <- file.path(main, subs)
  normalizePath(fpath, mustWork = FALSE)
}

#' @rdname paths
#'
#' @export
#'
raw_path <- function(main = ".", arg_checks = TRUE){
  sub_path(main = main, sub = "raw", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
casts_path <- function(main = ".", arg_checks = TRUE){
  sub_path(main = main, sub = "casts", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
data_path <- function(main = ".", arg_checks = TRUE){
  sub_path(main = main, sub = "data", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
models_path <- function(main = ".", arg_checks = TRUE){
  sub_path(main = main, sub = "models", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
tmp_path <- function(main = ".", arg_checks = TRUE){
  sub_path(main = main, sub = "tmp", arg_checks = arg_checks)
}

#' @rdname paths
#'
#' @export
#'
file_path <- function(main = ".", sub = NULL, files = NULL, 
                       arg_checks = TRUE){
  check_args(arg_checks)
  return_if_null(files)
  sub <- ifnull(sub, "")
  fpath <- file.path(main, sub, files)

  normalizePath(fpath, mustWork = FALSE)
}
