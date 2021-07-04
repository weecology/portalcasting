
#' @title Determine the path for a specific level of a portalcasting directory
#'
#' @description Produce paths for a portalcasting directory. 
#'              \cr \cr
#'              \code{main_path} returns the path for the \code{main} folder.
#'              \cr \cr
#'              \code{sub_path} returns the path(s) for the \code{subs}
#'              folder(s). \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'              the directory tree. 
#'
#' @param sub \code{character} vector of the name(s) of the sub component(s)
#'             of the directory tree. 
#'
#' @return \code{character} \code{\link[port{normalized_file_path}}(s).
#'          \cr \cr
#'          \code{main_path} path of the \code{main} folder.
#'          \cr \cr
#'          \code{subs_path} paths of the \code{sub} folder(s). 
#'
#' @name paths
#'
NULL

#' @rdname paths
#'
#' @export
#'
main_path <- function (main = ".") {

  if (length(main) > 1) {

    stop("`main` must be length-1")

  }

  normalized_file_path(main,
                       mustWork = FALSE)

}

#' @rdname paths
#'
#' @export
#'
sub_path <- function (main = ".", sub = NULL) {

  if (length(main) > 1) {

    stop("`main` must be length-1")

  }

  normalized_file_path(main, sub,
                       mustWork = FALSE)

}

#' @rdname paths
#'
#' @export
#'
resources_path <- function (main = ".") {

  sub_path(main = main, 
           sub = "resources")

}

#' @rdname paths
#'
#' @export
#'
casts_path <- function (main = ".") {

  sub_path(main = main, 
           sub = "casts")

}

#' @rdname paths
#'
#' @export
#'
fits_path <- function (main = ".") {

  sub_path(main = main, 
           sub = "fits")

}

#' @rdname paths
#'
#' @export
#'
data_path <- function (main = ".") {

  sub_path(main = main, 
           sub = "data")

}

#' @rdname paths
#'
#' @export
#'
models_path <- function (main = ".") {

  sub_path(main = main, 
           sub = "models")

}

