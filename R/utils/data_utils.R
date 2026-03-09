#' @title Round an Interpolated Series
#'
#' @description Wraps [`round`] around [`forecast::na.interp`] to provide a rounded interpolated series, which is then enforced to be greater than or equal to a minimum value (default `min_val = 0`) via [`pmax`].
#'
#' @param x A time series passed directly to [`forecast::na.interp`].
#'
#' @param lambda Box-Cox transformation parameter passed directly to [`forecast::na.interp`].
#'
#' @param linear `logical` indicator of if linear interpolation should be used. Passed directly to [`forecast::na.interp`].
#'
#' @param digits `integer` or `numeric` integer of how many digits to round to. Passed directly to [`round`].
#'
#' @param min_val `integer` or `numeric` integer of minimum value allowable in the series.
#'
#' @return `numeric` series.
#'
#' @family utilities
#'
#' @name round_na.interp
#' 
#' @examples
#'    round_na.interp(x = c(1, 2, 3, NA, NA, 170))
#'    round_na.interp(x = c(-1, 2, 3, NA, NA, 170), min_val = 1)
#'
NULL

#' @rdname round_na.interp
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


#' @title Create a Named Empty List
#'
#' @description Produces a list with `NULL` for each element named according to `element_names`.
#' 
#' @param element_names `character` vector of names for the elements in the list.
#'
#' @return `list` with names `element_names` and values `NULL`.
#'
#' @name named_null_list 
#'
#' @family utilities
#'
#' @examples
#'    named_null_list(c("a", "b", "c"))
#'
NULL

#' @rdname named_null_list 
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
#' @name update_list
#'
#' @family utilities
#'
#' @examples
#'    orig_list <- list(a = 1, b = 3, c = 4)
#'    update_list(orig_list)
#'    update_list(orig_list, a = "a")
#'    update_list(orig_list, a = 10, b = NULL)
#'
NULL

#' @rdname update_list
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
#' @name foy
#'
#' @family utilities
#'
#' @examples
#'    Sys.Date( )
#'    foy(Sys.Date())
#'
NULL

#' @rdname foy
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
#' @name ifnull
#'
#' @family utilities
#'
#' @examples
#'    ifnull(NULL, 123)
#'    ifnull(TRUE, 123)
#'    ifnull(FALSE, 123)
#'
NULL

#' @rdname ifnull
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
