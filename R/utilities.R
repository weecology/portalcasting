#' @title Round an Interpolated Series
#'
#' @description Wraps \code{\link{round}} around \code{\link[forecast]{na.interp}} to provide a rounded interpolated series, which is then enforced to be greater than or equal to a minimum value (default \code{min_val = 0}) via \code{\link{pmax}}.
#'
#' @param x A time series passed directly to \code{\link[forecast]{na.interp}}.
#'
#' @param lambda Box-Cox transformation parameter passed directly to \code{\link[forecast]{na.interp}}.
#'
#' @param linear \code{logical} indicator of if linear interpolation should be used. Passed directly to \code{\link[forecast]{na.interp}}.
#'
#' @param digits \code{integer} or \code{numeric} integer of how many digits to round to. Passed directly to \code{\link{round}}.
#'
#' @param min_val \code{integer} or \code{numeric} integer of minimum value allowable in the series.
#'
#' @return \code{numeric} series.
#' 
#' @examples
#'   round_na.interp(x = c(1, 2, 3, NA, NA, 170))
#'   round_na.interp(x = c(-1, 2, 3, NA, NA, 170), min_val = 1)
#'
#' @export
#'
round_na.interp <- function (x,
                             lambda  = NULL, 
                             linear  = (frequency(x) <= 1 | sum(!is.na(x)) <= 2 * frequency(x)),
                             digits  = 0,
                             min_val = 0) {

  xi <- na.interp(x      = x, 
                  lambda = lambda,
                  linear = linear)

  xr <- round(x      = xi,
              digits = digits)

  pmax(... = min_val,
       ... = xr)

}



#' @title Determine a File's Extension 
#'
#' @description Based on the separating character, determine the file extension.
#'
#' @param path \code{character} value of the file path possibly with an extension.
#'
#' @param sep_char \code{character} value of the separator that delineates the extension from the file path. \cr 
#'        Generally, this will be \code{"."}, but for some API URLs, the extension is actually a query component, so the separator may sometimes need to be \code{"="}.
#'
#' @return \code{character} value of the extension (\code{file_ext}).
#' 
#' @examples
#'  file_ext("home/folders.with.dots/stuff/ok.csv")
#'  file_ext(NMME_urls()[[1]])
#'  file_ext(NMME_urls()[[1]], "=")
#'
#' @export
#'
file_ext <- function (path, sep_char = ".") {
  
  for_regexpr <- paste0("\\", sep_char, "([[:alnum:]]+)$")
  pos         <- regexpr(for_regexpr, path)

  ifelse(pos > -1L, substring(path, pos + 1L), "")

}


#' @title Create a Named Empty List
#'
#' @description Produces a list with \code{NULL} for each element named according to \code{element_names}.
#' 
#' @param element_names \code{character} vector of names for the elements in the list.
#'
#' @return \code{list} with names \code{element_names} and values \code{NULL}.
#'
#' @examples
#'  named_null_list(c("a", "b", "c"))
#'
#' @export
#'
named_null_list <- function (element_names = NULL) {

  return_if_null(element_names)

  nelements  <- length(element_names)
  out        <- vector("list", nelements)
  names(out) <- element_names

  out

}

#' @title Update a List's Elements
#'
#' @description Update a list with new values for elements
#'
#' @param list \code{list} to be updated with \code{...}. 
#'
#' @param ... Named elements to update in \code{list}
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
update_list <- function (list = list(),
                                ...) {

  if (!is.list(list)) {

    stop("`list` must be a list")

  } 

  update_elems <- list(...)

  nupdate_elems <- length(update_elems)
  norig_elems   <- length(list)

  updated_list <- named_null_list(element_names = names(list))

  if (norig_elems > 0) {

    for (i in 1:norig_elems) {

      if (!is.null(list[[i]])) {

        updated_list[[i]] <- list[[i]]

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


#' @title Calculate the Fraction of the Year from a Date
#' 
#' @description Based on the year in which the date occurred, determine the fraction of the year (foy) for the date (in relation to New Year's Eve in that year). 
#'
#' @param dates \code{Date}(s) or \code{Date}-conformable value(s) to be converted to the fraction of the year.
#'
#' @return \code{numeric} value(s) of the fraction of the year.
#'
#' @examples
#'  foy(Sys.Date())
#'
#' @export
#'
foy <- function (dates = NULL) {

  return_if_null(dates)
  
  dates   <- as.Date(dates)
  jday    <- as.numeric(format(dates, "%j"))
  nye     <- as.Date(paste0(format(dates, "%Y"), "-12-31"))
  nyejday <- as.numeric(format(nye, "%j"))

  round(jday / nyejday, 3)

}




#' @title Replace a Value with an Alternative if it is NULL 
#'
#' @description Replaces the focal input with the alternative value if it is \code{NULL}. 
#'
#' @param x Focal input.
#'
#' @param alt Alternative value.
#'
#' @return \code{x} if not \code{NULL}, \code{alt} otherwise. 
#' 
#' @examples
#'  ifnull(NULL, 123)
#'  ifnull(TRUE, 123)
#'  ifnull(FALSE, 123)
#'
#' @name alternative values
#'
#' @export 
#'
ifnull <- function (x = NULL, alt = NULL) {

  if (is.null(x)) {
    alt
  } else {
    x
  }

}



