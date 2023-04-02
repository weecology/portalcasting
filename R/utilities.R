#' @title Find an Object's Host Package and Version Information
#'
#' @description Locate basic package information of an R object. If nothing is input, it operates on itself. \cr
#'   If the object is sourced through multiple packages, each package and its version are included.
#'
#' @param what An R object.
#'
#' @return `list` of the object, its class, the packages it is sourced from / through, and the versions of those packages.
#'
#' @export
#'
package_version_finder <- function (what) {

  if (missing(what)) {

    what <- "package_version_finder"

  }

  object_expr       <- parse(text         = what)
  object_eval       <- eval(expr          = object_expr)
  object_class      <- class(x            = object_eval)
  location_name     <- find(what          = what)
  packages_names    <- sapply(X           = location_name,
                              FUN         = gsub,
                              pattern     = "package\\:",
                              replacement = "")
  packages_versions <- sapply(X           = packages_names,
                              FUN         = packageDescription,
                              fields      = "Version")
  
  names(packages_versions) <- packages_names

  list(object   = what,
       class    = object_class,
       package  = packages_names,
       version  = packages_versions)

}


#' @title Round an Interpolated Series
#'
#' @description Wraps [`round`] around [`forecast::na.interp`] to provide a rounded interpolated series, which is then enforced to be greater than or equal to a minimum value (default `min_val = 0`) via [`pmax`].
#'
#' @param x A time series passed directly to [`forecast::na.interp`].
#'
#' @param lambda Box-Cox transformation parameter passed directly to [`forecast::na.interp`].
#'
#' @param linear `logic` indicator of if linear interpolation should be used. Passed directly to [`forecast::na.interp`].
#'
#' @param digits `integer` or `numeric` integer of how many digits to round to. Passed directly to [`round`].
#'
#' @param min_val `integer` or `numeric` integer of minimum value allowable in the series.
#'
#' @return `numeric` series.
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
#' @param path `character` value of the file path possibly with an extension.
#'
#' @param sep_char `character` value of the separator that delineates the extension from the file path. \cr 
#'        Generally, this will be `."`, but for some API URLs, the extension is actually a query component, so the separator may sometimes need to be `"="`.
#'
#' @return `character` value of the extension (`file_ext`).
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
#' @description Produces a list with `NULL` for each element named according to `element_names`.
#' 
#' @param element_names `character` vector of names for the elements in the list.
#'
#' @return `list` with names `element_names` and values `NULL`.
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
#' @param list `list` to be updated with `...`. 
#'
#' @param ... Named elements to update in `list`
#'
#' @return Updated `list`.
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
#' @param dates `Date`(s) or `Date`-conformable value(s) to be converted to the fraction of the year.
#'
#' @return `numeric` value(s) of the fraction of the year.
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
#' @description Replaces the focal input with the alternative value if it is `NULL`. 
#'
#' @param x Focal input.
#'
#' @param alt Alternative value.
#'
#' @return `x` if not `NULL`, `alt` otherwise. 
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



