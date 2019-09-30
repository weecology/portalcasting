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
#'   generated. If \code{NULL}, it is as if \code{TRUE}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
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
  check_args(arg_checks = arg_checks)
  return_if_null(msg)
  return_if_null(quiet)
  if (!quiet){
    msg2 <- paste(msg, collapse = "\n")
    message(msg2)
  }
  invisible(NULL)
}



#' @title Produce a horizontal break line for messaging
#'
#' @description Optionally creates and returns a message that is a
#'  horizontal break line of dashes.
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  made or not. For toggling separately from the more general \code{quiet}
#'  argument. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#' 
#' @return \code{NULL} (message is put out to console).
#'
#' @examples
#'  messageq_break()
#'
#' @export
#'
messageq_break <- function(bline = TRUE, quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  if(!bline){
    return()
  }
  msg <- paste(rep("-", 60), collapse = "") 
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
}

#' @title Specific message functions
#'
#' @description Messaging functions:
#'  \code{version_message} creates a message for the local package  
#'   version. \cr \cr
#'  \code{portalcast_welcome} generates a welcome message for a
#'   portalcast run. \cr \cr
#'  \code{portalcast_goodbye} makes a goodbye message for when models are 
#'   done. \cr \cr
#'  \code{sandbox_welcome} generates a special welcome message for sandboxes,
#'   displayed upon completion of \code{\link{setup_sandbox}}. \cr \cr
#'  \code{creation_message} makes a message about the creation of the
#'   directory. \cr \cr
#'  \code{setup_completion_message} creates a message for when the directory
#'   setting up has successfully finished. \cr \cr
#'  \code{fill_casts_message} creates the final message to be used in 
#'   \code{\link{fill_casts}} that relays the number, and optionally the
#'   specific names, of the files moved.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if message should be quieted.
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  included.
#'
#' @param end_moon \code{integer} (or integer \code{numeric}) 
#'  newmoon number of the last sample(s) to be included.
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
#' @param run_status Output from the \code{\link{source}}-ing of a model
#'  script; a \code{list} if successful or \code{NA} if not.
#'
#' @param model \code{character} value of the model name or model script path.
#'  Will be stripped of all excess path components and the extension to
#'  give just the name of the model itself. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#' 
#' @return Message is put to the console; \code{NULL} is returned.
#'
#' @examples
#'  portalcast_welcome()
#'  portalcast_goodbye()
#'  sandbox_welcome()
#'  creation_message()
#'  setup_completion_message()
#'  data_readying_message(end_moon = 400)
#'  data_resetting_message()
#'  models_running_message(end_moon = 400)
#'  model_running_message("ESSS")
#'  model_done_message("ESSS", list("output"))
#'  model_done_message("ESSS", NA)
#'  fill_casts_message("xx.csv", TRUE)
#'  fill_casts_message(c("xx.csv", "yy.R"), c(TRUE, FALSE), 
#'                           verbose = TRUE)
#'
#' @name portalcast_messages
#'
#'
NULL

#' @rdname portalcast_messages
#'
#' @export
#' 
version_message <- function(bline = TRUE, quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  version_number <- packageDescription("portalcasting", fields = "Version")
  msg <- paste0("This is portalcasting v", version_number)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}

  

#' @rdname portalcast_messages
#'
#' @export
#' 
portalcast_welcome <- function(bline = TRUE, quiet = FALSE,
                               arg_checks = TRUE){
  check_args(arg_checks)
  version_message(bline = bline, quiet = quiet, arg_checks = arg_checks)
  msg <- "Preparing directory for casting"
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}

#' @rdname portalcast_messages
#'
#' @export
#'
portalcast_goodbye <- function(bline = TRUE, quiet = FALSE,
                               arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  msg <- "Cating complete"
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  messageq(msg, quiet)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}


#' @rdname portalcast_messages
#'
#' @export
#' 
sandbox_welcome <-function(main = ".", quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks)

  main <- main_path(main = main, arg_checks = arg_checks)
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
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
creation_message <- function(main = ".", bline = TRUE, quiet = FALSE, 
                             arg_checks = TRUE){
  main_path <- main_path(main = main, arg_checks = arg_checks)
  msg <- paste0("Establishing portalcasting directory at\n ", main_path)
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
setup_completion_message <- function(bline = TRUE, quiet = FALSE, 
                                     arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  msg <- "Directory successfully instantiated"
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
data_readying_message <- function(end_moon = NULL, bline = TRUE, 
                                  quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(end_moon)
  msg <- paste0("Readying data for forecast origin newmoon ", end_moon)

  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}


#' @rdname portalcast_messages
#'
#' @export
#' 
data_resetting_message <- function(bline = TRUE, quiet = FALSE,
                                   arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  msg <- "Resetting data to the most up-to-date versions"

  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}




#' @rdname portalcast_messages
#'
#' @export
#' 
models_running_message <- function(end_moon = NULL, bline = TRUE, 
                                   quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(end_moon)
  msg <- paste0("Running models for forecast origin newmoon ", end_moon)
  messageq_break(bline = bline, quiet = quiet, arg_checks = arg_checks)
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
  invisible(NULL)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
model_running_message <- function(model = NULL, quiet = FALSE, 
                                  arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  return_if_null(model)
  modelname <- path_no_ext(basename(model))
  messageq(paste0(" -Running ", modelname), quiet)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
model_done_message <- function(model = NULL, run_status = NULL, quiet = FALSE, 
                               arg_checks = TRUE){

  check_args(arg_checks = arg_checks)
  return_if_null(model)
  return_if_null(run_status)
  modelname <- path_no_ext(basename(model))

  if(all(is.na(run_status))){
    msg <- paste0("  |----| ", modelname, " failed |----|")
  } else{
    msg <- paste0("  |++++| ", modelname, " successful |++++|")
  }
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
}

#' @rdname portalcast_messages
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
  messageq(msg = msg, quiet = quiet, arg_checks = arg_checks)
}





