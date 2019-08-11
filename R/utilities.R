
#' @title Save data out to a file and return it	
#'
#' @description Save inputted data out to a data file if requested and 
#'  return it to the console.
#'
#' @param x \code{data.frame} table to be written out.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param save \code{logical} indicator controlling if \code{x} should 
#'   be saved out.
#'
#' @param filename \code{character} name of the file for saving \code{x}.
#'
#' @param overwrite \code{logical} indicator of if the file should be
#'  overwritten if it exists.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{x} as input.
#'
#'
#' @export
#'
data_out <- function(x = NULL, main = ".", save = TRUE, filename = "x.csv", 
                     overwrite = TRUE, quiet = FALSE){
  return_if_null(x)
  if(save){
    local_path <- paste0("data/", filename)
    full_path <- file_paths(main, local_path)
    f_exists <- file.exists(full_path)
    if(f_exists){
      if(overwrite){
        write.csv(x, full_path, row.names = FALSE)
        msg <- paste0(filename, " exists and overwrite = TRUE; file saved")
      } else {
        msg <- paste0(filename, " exists and overwrite = FALSE; not saved") 
      }
    } else{
      write.csv(x, full_path, row.names = FALSE)
      msg <- paste0(filename, " saved")
    }
    messageq(msg, quiet)
  }
  x
}

#' @title If a value is NULL, trigger the parent function's return
#'
#' @description If the focal input is \code{NULL}, return \code{value} from
#'  the parent function. Should only be used within a function.
#'
#' @param x Focal input.
#'
#' @param value If \code{x} is \code{NULL}, \code{\link{return}} this input
#'  from the parent function. 
#'
#' @return If \code{x} is not \code{NULL}, \code{NULL} is returned. If 
#'  \code{x} is \code{NULL}, the result of \code{\link{return}} with 
#'  \code{value} as its input evaluated within the parent function's 
#'  environment is returned.
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
return_if_null <- function(x, value = NULL){
  if(is.null(x)){
    do.call(return, list(value), envir = sys.frame(-1))
  } 
}

#' @title Determine the depth of a list
#'
#' @description Evaluate an input for the depth of its nesting. 
#'
#' @details If \code{x = list()}, then technically the input value is a list, 
#'  but is empty (of length \code{0}), so depth is returned as \code{0}.
#'
#' @param x Focal input.
#'
#' @return \code{integer} value of the depth of the list.
#' 
#' @examples
#'  list_depth("a")
#'  list_depth(list())
#'  list_depth(list("a"))
#'  list_depth(list(list("a")))
#'
#' @export 
#'
list_depth <- function(x){
  xx <- match.call()
  xxx <- deparse(xx[[2]])
  if(xxx == "list()"){
    0L
  } else if (is.list(x)){
    1L + max(sapply(x, list_depth))
  } else {
    0L
  }
}

#' @title Replace if NULL
#'
#' @description If the focal input is \code{NULL}, replace it with 
#'   alternative. 
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
#' @export 
#'
ifnull <- function(x = NULL, alt = NULL){
  if(is.null(x)){
    x <- alt
  }
  x
}

#' @title Optionally generate a message based on a logical input
#'
#' @description Given the input to \code{quiet}, generate the message(s) 
#'   in \code{msg} or not.
#'
#' @param msg \code{character} vector of the message(s) to generate or 
#'   \code{NULL}. If more than one element is contained in \code{msg}, they
#'   are concatenated with a newline between.
#'
#' @param quiet \code{logical} indicator controlling if the message is
#'   generated.
#'
#' @examples
#'  messageq("Hello world", FALSE)
#'  messageq("Hello world", TRUE)
#'
#' @export
#'
messageq <- function(msg = NULL, quiet = FALSE){
  if (!quiet){
    msg2 <- paste(msg, collapse = "\n")
    message(msg2)
  }
}