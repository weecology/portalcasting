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
#' @param arg_checks \code{logical} value of if the arguments should be
#'   checked using standard protocols via \code{\link{check_args}}. The 
#'   default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'   formatted correctly and provides directed error messages if not. \cr
#'   However, in sandboxing, it is often desirable to be able to deviate from 
#'   strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'   many/most/all enclosed functions to not check any arguments using 
#'   \code{\link{check_args}}, and as such, \emph{caveat emptor}.
#'
#' @return A message is given, and \code{NULL} returned.
#'
#' @examples
#'  messageq("Hello world", FALSE)
#'  messageq("Hello world", TRUE)
#'
#' @export
#'
messageq <- function(msg = NULL, quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  if (!quiet){
    msg2 <- paste(msg, collapse = "\n")
    message(msg2)
  }
  invisible(NULL)
}


#' @title Create the final message for filling casts
#'
#' @description Create the final message to be used in 
#'  \code{\link{fill_casts}} that relays the number, and optionally the
#'  specific names, of the files moved.
#'
#' @param files \code{character} vector of the base file names of the files
#'  that may or may not have been moved.
#'
#' @param movedTF \code{logical} vector indicating if each of the files in 
#'  \code{files} has been moved or not.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'   all of the files and whether they were moved or not.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return A message is given, and \code{NULL} returned.
#'
#' @examples
#'  fill_casts_message("xx.csv", TRUE)
#'  fill_casts_message(c("xx.csv", "yy.R"), c(TRUE, FALSE), 
#'                           verbose = TRUE)
#'
#' @export
#'
fill_casts_message <- function(files = NULL, movedTF = NULL, quiet = FALSE,
                               verbose = FALSE, arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  return_if_null(files)
  moved <- basename(files[movedTF])
  not_moved <- basename(files[!movedTF])
  n_moved <- length(moved)
  n_not_moved <- length(not_moved)
  msg1 <- NULL
  msg2 <- NULL
  if(n_moved > 0){
    msg1 <- c(msg1, "moved:", moved)
    msg2 <- paste0(msg2, n_moved, " casts files moved")
  }
  if(n_not_moved > 0){
    msg1 <- c(msg1, "not moved: ", not_moved)
    if(n_moved > 0){
      msg2 <- paste(msg2, ", ")
    }
    msg2 <- paste0(msg2, n_moved, " casts files not moved")
  }
  if(!verbose){   
    msg1 <- NULL 
  }
  
  msg <- c(msg1, paste0("   **", msg2, "**"))
  messageq(msg, quiet)
  invisible(NULL)
}


#' @title Directory welcome and model completion messages
#'
#' @description Messaging functions for top-level message output.
#'  \code{portalcast_welcome} creates a welcome message for the directory 
#'   based on the package version and directory type. \cr \cr
#'  \code{portalcast_goodbye} creates a goodbye message for when models are 
#'   done. \cr \cr
#'  \code{sandbox_welcome} creates a welcome message for sandbox users,
#'   displayed upon completion of \code{\link{setup_sandbox}}. \cr \cr
#'
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if message should be quieted.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#' 
#' @return \code{NULL}, as message is printed.
#'
#' @examples
#'  portalcast_welcome()
#'  portalcast_goodbye()
#'  sandbox_welcome()
#'
#' @name portalcast_messages
#'
#'
NULL

#' @rdname portalcast_messages
#'
#' @export
#' 
portalcast_welcome <- function(quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  msg1 <- "---------------------------------------------------------"
  version_number <- packageDescription("portalcasting", fields = "Version")
  msg2 <- paste0("This is portalcasting v", version_number)
  messageq(c(msg1, msg2, msg1), quiet)
}

#' @rdname portalcast_messages
#'
#' @export
#'
portalcast_goodbye <- function(quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)
  msg1 <- "---------------------------------------------------------"
  messageq(c(msg1, "Models done", msg1), quiet)
}


#' @rdname portalcast_messages
#'
#' @export
#' 
sandbox_welcome <-function(main = ".", quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)

main <- main_path(main)
castle <- "
                                         ____
             /\\                         / -- )   
            /  \\                       (____/
           /|  |\\                       / /  
          /_|__|_\\                     / / 
          |      |                    / /
 __    __ |      | __    __          / / 
[  ]__[  ].      .[  ]__[  ]        / /  
|__         ____         __|  ____ / /__ 
   |      .|    |.      |    / .------  )
   |      |      |      |   / /      / / 
   |      |      |      |  / /      / /  
~~~~~~~~~~~~~~~~~~~~~~~~~~------------~~~~~~~~~~~~~~
"
succ <- "sanbox directory successfully set up at \n  "
happy <- "\nHappy portalcasting!"
msg <- paste0(castle, succ, main, happy)

  messageq(msg, quiet)
}


