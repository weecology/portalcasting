#' @title Determine a File's Extension 
#'
#' @description Based on the separating character, determine the file extension.
#'
#' @param path `character` value of the file path possibly with an extension.
#'
#' @param sep_char `character` value of the separator that delineates the extension from the file path. \cr 
#'        Generally, this will be `."`, but for some API URLs, the extension is actually a query component, so the separator may sometimes need to be `"="`.
#'
#' @return `character` value of the extension (`file_ext`).
#'
#' @name file_ext
#' 
#' @family utilities
#'
#' @examples
#'    file_ext("home/folders.with.dots/stuff/ok.csv")
#'    file_ext(NMME_urls()[[1]])
#'    file_ext(NMME_urls()[[1]], "=")
#'
NULL

#' @rdname file_ext
#'
#' @export
#'
file_ext <- function (path, sep_char = ".") {
  
  for_regexpr <- paste0("\\", sep_char, "([[:alnum:]]+)$")
  pos         <- regexpr(for_regexpr, path)

  ifelse(pos > -1L, substring(path, pos + 1L), "")

}
