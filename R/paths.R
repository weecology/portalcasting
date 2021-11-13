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
  
  for_regexpr <- paste0("\\", sep_char, "([[:alnum:]]+)$")
  pos <- regexpr(for_regexpr, path)
  ifelse(pos > -1L, substring(path, pos + 1L), "")
}

#' @rdname file_ext
#'
#' @export
#'
path_no_ext <- function(path, sep_char = "."){
  
  for_sub <- paste0("([^", sep_char, "]+)\\.[[:alnum:]]+$")
  sub(for_sub, "\\1", path)
}


