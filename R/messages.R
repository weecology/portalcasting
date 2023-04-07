#' @title Functions for Message Generation 
#'
#' @description Create messages for use in the portalcasting pipeline. 
#'   `messageq`: Optionally generate a message based on a logical input. Uses a wrapper on [`message`][base::message] that, given the input to `quiet`, generates the message(s) in `...` or not. \cr \cr
#'   `break_line`: Creates a horizontal line of characters ending with a newline call for messages. \cr \cr
#'   `break_lines`: Creates a set of horizontal line of characters ending with a newline call for messages. \cr \cr
#'   `castle`: Creates a text drawing of a sandcastle of characters for messages. 
#'
#' @param ... zero or more objects that can be coerced to `character` and are concatenated with no separator added, or a single condition object. See [`message`][base::message].
#'
#' @param quiet `logic` indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If `NA`, messages will not be translated. See [`message`][base::message] and [`base::gettext`].
#'
#' @param appendLF `logic` indicator if messages given as a `character` string should have a newline appended. See [`message`][base::message].
#'
#' @param char `character` value to repeated ``reps` times to form the break line. 
#'
#' @param reps `integer`-conformable value for number of times `char` is replicated.
#' 
#' @param nlines `integer`-conformable value for the number of [`break_line`]s to include. Defaults to `2` lines.
#'
#' @return 
#'   `messageq`: A message is given, and `NULL` returned, [`invisible`][base::invisible]-ly. \cr \cr 
#'   `break_line`: The `character` of the line to be passed to [`message`][base::message] or [`messageq`]. \cr \cr 
#'   `break_lines`: The `character` of the lines to be passed to [`message`][base::message] or [`messageq`]. \cr \cr 
#'   `castle`: The `character` of the sandcastle to be passed to [`message`][base::message] or [`messageq`]. 
#'
#' @name messages
#'
#' @examples 
#'    messageq("hi ", "how are you?")
#'    messageq("hi ", "how are you?", quiet = TRUE)
#'    break_line( )
#'    break_lines( )
#'    castle( )
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
                                            
             /\\                           ______
            /  \\                         /.---- )
           /|  |\\                       //    //
          /_|__|_\\                     (_----_/ 
          |      |                       / /              
 __    __ |      | __    __             / / 
[  ]__[  ].      .[  ]__[  ]           / /     
|__         ____         __|      ____/ /__    
   |      .|    |.      |        / .-----  )   
   |      |      |      |       / /     / /    
   |      |      |      |      / /     / /     
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----------------~~~~~~~
"

}

