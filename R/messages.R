#' @title Functions for Message Generation 
#'
#' @description 
#'   \code{messageq}: Optionally generate a message based on a logical input. Uses a wrapper on \code{\link[base]{message}} that, given the input to \code{quiet}, generates the message(s) in \code{...} or not. \cr \cr
#'   \code{break_line}: Creates a horizontal line of characters ending with a newline call for messages. \cr \cr
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
#'
#' @return 
#'   \code{messageq}: A message is given, and \code{NULL} returned, \code{\link[base]{invisible}}-ly. \cr
#'   \code{break_line}: The \code{character} of the line to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   \code{castle}: The \code{character} of the sandcastle to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'
#' @name messages
#'
NULL

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
  
  paste(rep(char, reps), "\n", collapse = "") 

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