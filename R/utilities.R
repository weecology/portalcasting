
#' @title Replace a value with an alternative if it is NULL or if it is NA
#'
#' @description 
#'  \code{ifnull} replaces the focal input with the alternative value if it
#'   is \code{NULL}. \cr \cr
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
#'  ifna(NA, 123)
#'  ifna(FALSE, 123)
#'  ifna(NA, NA)
#'
#' @name alternative_values
#'
NULL

#' @rdname alternative_values
#'
#' @export 
#'
ifnull <- function (x = NULL, alt = NULL) {

  if(is.null(x)){
    x <- alt
  }
  x

}


#' @title Optionally generate a message based on a logical input
#'
#' @description A wrapper on \code{\link[base]{message}} that, given the input to \code{quiet}, generates the message(s) in \code{...} or not.
#'
#' @param ... zero or more objects that can be coerced to \code{character} and are concatenated with no separator added, or a single condition object. See \code{\link[base]{message}}.
#'
#' @param quiet \code{logical} indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If \code{NA}, messages will not be translated. See \code{\link[base]{message}} and \code{\link[base]{gettext}}.
#'
#' @param appendLF \code{logical} indicator if messages given as a \code{character} string should have a newline appended. See \code{\link[base]{message}}.
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


#' @title Produce a horizontal break line for messaging
#'
#' @description Creates a horizontal line of characters for messages.
#'
#' @param char \code{character} value to repeated \code{reps} times to form
#'             the break. 
#'
#' @param reps \code{integer}-conformable value for number of times
#'             \code{char} is replicated.
#' 
#' @return \code{NULL} (message is put out to console).
#'
#' @examples
#'  message_break()
#'
#' @export
#'
message_break <- function(char = "-",
                          reps = 60){
  
  paste(rep(char, reps), collapse = "") 

}

#' @title Create a Named Empty List
#'
#' @description Produces a \code{list} with \code{NULL} for each element that is named according to \code{element_names}.
#' 
#' @param element_names \code{character} vector of names for the elements in the list.
#'
#' @return \code{list} of \code{length(element_names)} elements with names \code{element_names} and values \code{NULL}.
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

#' @title Add a Newmoon Number Column to a Table that has a Date Column 
#' 
#' @description Add a \code{newmoonnumber} column to a table that has a \code{date} column.
#' 
#' @param df \code{data.frame} with column of \code{date}s.
#'
#' @param moons Moons \code{data.frame}. See \code{\link{prep_moons}}.
#'
#' @return \code{data.frame} \code{df} with column of \code{newmoonnumber}s added.
#'
#' @export
#'
add_newmoonnumbers_from_dates <- function (df, moons = NULL) {

  return_if_null(moons, df)

  if (is.null(df$date)) {

    df <- add_date_from_components(df)
  }

  moon_number       <- moons$newmoonnumber[-1]
  moon_start        <- as.Date(moons$newmoondate[-nrow(moons)])
  moon_end          <- as.Date(moons$newmoondate[-1])
  moon_match_number <- NULL
  moon_match_date   <- NULL

  for (i in seq(moon_number)) {

    temp_dates        <- seq.Date(moon_start[i] + 1, moon_end[i], 1)
    temp_dates        <- as.character(temp_dates)
    temp_numbers      <- rep(moon_number[i], length(temp_dates))
    moon_match_date   <- c(moon_match_date, temp_dates)
    moon_match_number <- c(moon_match_number, temp_numbers)

  }

  moon_match_date  <- as.Date(moon_match_date)
  moon_matches     <- match(df$date, moon_match_date)
  df$newmoonnumber <- moon_match_number[moon_matches]

  df

}


#' @title Update a list's elements
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





#' @title If a value is NULL, trigger the parent function's return
#'
#' @description If the focal input is \code{NULL}, return \code{value} from the parent function. \cr \cr 
#'              Should only be used within a function.
#'
#' @param x Focal input.
#'
#' @param value If \code{x} is \code{NULL}, \code{\link{return}} this input from the parent function. 
#'
#' @return If \code{x} is not \code{NULL}, \code{NULL} is returned. If \code{x} is \code{NULL}, the result of \code{\link{return}} with \code{value} as its input evaluated within the parent function's environment is returned.
#' 
#' @examples
#'  ff <- function(x = 1, null_return = "hello"){
#'    return_if_null(x, null_return)
#'    x
#'  }
#'  ff()
#'  ff(NULL)
#'
#' @export 
#'
return_if_null <- function (x, value = NULL) {

  if (is.null(x)) {

    do.call(return, list(value), envir = sys.frame(-1))

  } 

}

#' @title Determine a File's Extension or Remove the Extension from the Path
#'
#' @description Based on the separating character (\code{sep_char}), \code{file_ext} determines the file extension and \code{path_no_ext} determines the file path without the extension.
#'
#' @param path \code{character} value of the file path possibly with an extension.
#'
#' @param sep_char \code{character} value of the separator that delineates the extension from the file path. Generally, this will be \code{"."}, but for some API URLs, the extension is actually a query component, so the separator may sometimes need to be \code{"="}.
#'
#' @return \code{character} value of the extension (\code{file_ext}) or the path without the extension (\code{path_no_ext}.
#' 
#' @examples
#'  file_ext("home/folders.with.dots/stuff/ok.csv")
#'  path_no_ext("home/folders.with.dots/stuff/ok.csv")
#'  file_ext(NMME_urls()[[1]])
#'  file_ext(NMME_urls()[[1]], "=")
#'
#' @export
#'
file_ext <- function(path, 
                     sep_char = "."){
  
  pos <- regexpr(paste0("\\", sep_char, "([[:alnum:]]+)$"), path)
  ifelse(pos > -1L, substring(path, pos + 1L), "")

}

#' @rdname file_ext
#'
#' @export
#'
path_no_ext <- function(path, 
                        sep_char = "."){
  
  sub(paste0("([^", sep_char, "]+)\\.[[:alnum:]]+$"), "\\1", path)

}