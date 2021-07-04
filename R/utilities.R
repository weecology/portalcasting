#' @title Optionally generate a message based on a logical input
#'
#' @description A wrapper on \code{\link[base]{message}} that, given the
#'              input to \code{quiet}, generates the message(s) in \code{...}
#'              or not.
#'
#' @param ... zero or more objects that can be coerced to \code{character}
#'            and are concatenated with no separator added, or a single 
#'            condition object.
#'            \cr \cr
#'            See \code{\link[base]{message}}.
#'
#' @param quiet \code{logical} indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If \code{NA}, messages will
#'               not be translated.
#'               \cr \cr
#'               See \code{\link[base]{message}} and 
#'               \code{\link[base]{gettext}}.
#'
#' @param appendLF \code{logical} indicator if messages given as a 
#'                 \code{character} string should have a newline appended.
#'                 \cr \cr
#'                 See \code{\link[base]{message}}.
#'
#' @return A message is given, and \code{NULL} returned.
#'
#' @export
#'
messageq <- function (..., 
                      quiet    = FALSE, 
                      domain   = NULL, 
                      appendLF = TRUE) {

  if (!quiet) {

    message(...,
            domain   = domain,
            appendLF = appendLF)

  }

  invisible()
}




#' @title If a value is NULL, trigger the parent function's return
#'
#' @description If the focal input is \code{NULL}, return \code{value} from
#'              the parent function. Should only be used within a function.
#'
#' @param x Focal input.
#'
#' @param value If \code{x} is \code{NULL}, \code{\link{return}} this input
#'              from the parent function. 
#'
#' @return If \code{x} is not \code{NULL}, \code{NULL} is returned. If 
#'         \code{x} is \code{NULL}, the result of \code{\link{return}} with 
#'         \code{value} as its input evaluated within the parent function's 
#'         environment is returned.
#' 
#' @examples
#'  ff <- function(x = 1, null_return = "hello"){
#'    return_if_null(x, null_return)
#'    x
#'  }
#'  ff()
#'  ff(NULL)
#'  ff(NULL, null_return = "ok")
#'  ff(null_return = "ok")
#'
#' @export 
#'
return_if_null <- function (x, value = NULL) {

  if (is.null(x)) {

    do.call(return, list(value), envir = sys.frame(-1))

  } 

}




#' @title Update a list's elements
#'
#' @description Update a list with new values for elements
#'
#' @param orig_list \code{list} to be updated with \code{...}. 
#'
#' @param ... Named elements to update in \code{orig_list}
#'
#' @return Updated \code{list}.
#'
#' @examples
#'  orig_list <- list(a = 1, b = 3, c = 4)
#'  update_list(orig_list)
#'  update_list(orig_list, a = "a")
#'  update_list(orig_list, a = 10, b = NULL)
#'
#' @export
#'
update_list <- function (orig_list = list(), ...) {

  if (!is.list(orig_list)) {

    stop("orig_list must be a list", call. = FALSE)
  } 

  update_elems <- list(...)
  nupdate_elems <- length(update_elems)
  norig_elems <- length(orig_list)
  updated_list <- vector("list", length = norig_elems)
  names(updated_list) <- names(orig_list)

  if (norig_elems > 0) {

    for (i in 1:norig_elems) {

      if (!is.null(orig_list[[i]])) {

        updated_list[[i]] <- orig_list[[i]]
 
 
      }

    }

  }

  if (nupdate_elems > 0) {

    names_update_elems <- names(update_elems)

    for (i in 1:nupdate_elems) {

      if (!is.null(update_elems[[i]])) {

        updated_list[[names_update_elems[i]]] <- update_elems[[i]]

      }

    }

  }

  updated_list

}


