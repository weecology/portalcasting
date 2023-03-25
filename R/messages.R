#' @title Functions for Message Generation 
#'
#' @description Create messages for use in the portalcasting pipeline. 
#'   \code{messageq}: Optionally generate a message based on a logical input. Uses a wrapper on \code{\link[base]{message}} that, given the input to \code{quiet}, generates the message(s) in \code{...} or not. \cr \cr
#'   \code{break_line}: Creates a horizontal line of characters ending with a newline call for messages. \cr \cr
#'   \code{break_lines}: Creates a set of horizontal line of characters ending with a newline call for messages. \cr \cr
#'   \code{castle}: Creates a text drawing of a sandcastle of characters for messages. 
#'
#' @param ... zero or more objects that can be coerced to \code{character} and are concatenated with no separator added, or a single condition object. See \code{\link[base]{message}}.
#'
#' @param quiet \code{logical} indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If \code{NA}, messages will not be translated. See \code{\link[base]{message}} and \code{\link[base]{gettext}}.
#'
#' @param appendLF \code{logical} indicator if messages given as a \code{character} string should have a newline appended. See \code{\link[base]{message}}.
#'
#' @param char \code{character} value to repeated \code{reps} times to form the break line. 
#'
#' @param reps \code{integer}-conformable value for number of times \code{char} is replicated.
#' 
#' @param nlines \code{integer}-conformable value for the number of \code{\link{break_line}}s to include. Defaults to \code{2} lines.
#'
#' @return 
#'   \code{messageq}: A message is given, and \code{NULL} returned, \code{\link[base]{invisible}}-ly. \cr
#'   \code{break_line}: The \code{character} of the line to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   \code{break_lines}: The \code{character} of the lines to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   \code{castle}: The \code{character} of the sandcastle to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. 
#'
#' @name messages
#'
NULL

#' @rdname messages
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

  invisible( )

}

#' @rdname messages
#'
#' @export
#'
break_line <- function(char = "-",
                       reps = 60){
  
  paste0(paste(rep(char, reps), collapse = ""), "\n") 

}


#' @rdname messages
#'
#' @export
#'
break_lines <- function(nlines = 2,
                        char   = "-",
                        reps   = 60) {

  out <- character(0)

  for (i in 1:nlines) {

    out <- c(out, break_line(char = char,
                             reps = reps))

  }
  out 
}

#' @rdname messages
#'
#' @export
#'
castle <- function ( ) {

"
                                            
             /\\                         
            /  \\                          ____
           /|  |\\                        / -- )   
          /_|__|_\\                      (_--_/ 
          |      |                       / /              
 __    __ |      | __    __             / / 
[  ]__[  ].      .[  ]__[  ]           / /  
|__         ____         __|      ____/ /__ 
   |      .|    |.      |        / .-----  )
   |      |      |      |       / /     / / 
   |      |      |      |      / /     / /  
~~~~~~~~~~~~~~~~~~~~~~~~~~------------~~~~~~~~~~~~~~
"

}

