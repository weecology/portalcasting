#' @title Optionally generate a message based on a logical input
#'
#' @description A wrapper on \code{\link[base]{message}} that, given the
#'              input to \code{quiet}, generates the message(s) in \code{...}
#'              or not.
#'
#' @param ... zero or more objects that can be coerced to \code{character}
#'            and are concatenated with no separator added, or a single 
#'            condition object.
#'            \cr \cr
#'            See \code{\link[base]{message}}.
#'
#' @param quiet \code{logical} indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If \code{NA}, messages will
#'               not be translated.
#'               \cr \cr
#'               See \code{\link[base]{message}} and 
#'               \code{\link[base]{gettext}}.
#'
#' @param appendLF \code{logical} indicator if messages given as a 
#'                 \code{character} string should have a newline appended.
#'                 \cr \cr
#'                 See \code{\link[base]{message}}.
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
message_break <- function(char = "-", reps = 60){
  

  paste(rep(char, reps), collapse = "") 

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
#'  \code{fill_fits_message} creates the final message to be used in 
#'   \code{\link{fill_fits}} that relays the number, and optionally the
#'   specific names, of the files moved.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param quiet \code{logical} indicator if message should be quieted.
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
#'  fill_fits_message("xx.csv", TRUE)
#'
#' @name portalcast_messages
#'
#'
NULL

#' @rdname portalcast_messages
#'
#' @export
#' 
version_message <- function(quiet = FALSE){
  
  version_number <- packageDescription("portalcasting", fields = "Version")
  messageq(message_break(), quiet = quiet)
  messageq("This is portalcasting v", version_number, quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)
}

  

#' @rdname portalcast_messages
#'
#' @export
#' 
portalcast_welcome <- function(quiet = FALSE){

  version_message(quiet = quiet)
  messageq("Preparing directory for casting", quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)

}

#' @rdname portalcast_messages
#'
#' @export
#'
portalcast_goodbye <- function(quiet = FALSE){
  
  messageq(message_break(), quiet = quiet)
  messageq("Casting complete", quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)

}


#' @rdname portalcast_messages
#'
#' @export
#' 
sandbox_welcome <-function(main = ".", quiet = FALSE){

  main <- main_path(main = main)
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
  messageq(castle, succ, main, happy, quiet = quiet)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
creation_message <- function(main = ".", quiet = FALSE){
  mpath <- normalizePath(file.path(main = main), mustWork = FALSE)
  messageq("Establishing portalcasting directory at\n ", mpath, quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible()
}

#' @rdname portalcast_messages
#'
#' @export
#' 
setup_completion_message <- function(quiet = FALSE){
  
  messageq(message_break(), quiet = quiet)
  messageq("Directory successfully instantiated", quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
data_readying_message <- function(end_moon = NULL,  
                                  quiet = FALSE){
  
  return_if_null(end_moon)

  messageq("Readying data for forecast origin newmoon ", end_moon, 
quiet = quiet)
  invisible(NULL)
}


#' @rdname portalcast_messages
#'
#' @export
#' 
data_resetting_message <- function(quiet = FALSE){
  
  msg <- 

  messageq(message_break(), quiet = quiet)
  messageq("Resetting data to the most up-to-date versions", quiet = quiet)
  invisible(NULL)
}




#' @rdname portalcast_messages
#'
#' @export
#' 
models_running_message <- function(end_moon = NULL,  
                                   quiet = FALSE){
  
  return_if_null(end_moon)
  messageq(message_break(), quiet = quiet)
  messageq("Running models for forecast origin newmoon ", end_moon, 
           quiet = quiet)
  invisible(NULL)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
model_running_message <- function(model = NULL, quiet = FALSE){

  
  return_if_null(model)
  modelname <- path_no_ext(basename(model))
  messageq(" -Running ", modelname, quiet = quiet)
}

#' @rdname portalcast_messages
#'
#' @export
#' 
model_done_message <- function(model = NULL, run_status = NULL, 
                              quiet = FALSE){

  
  return_if_null(model)
  return_if_null(run_status)
  modelname <- path_no_ext(basename(model))

  if(all(is.na(run_status))){
    messageq("  |----| ", modelname, " failed |----|", quiet = quiet)
  } else{
    messageq("  |++++| ", modelname, " successful |++++|", quiet = quiet)
  }

}

#' @rdname portalcast_messages
#'
#' @export
#' 
fill_casts_message <- function(files = NULL, movedTF = NULL, quiet = FALSE,
                               verbose = FALSE){

  
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
  
  messageq(msg1, "   **", msg2, "**", quiet = quiet)
}



#' @rdname portalcast_messages
#'
#' @export
#' 
fill_fits_message <- function(files = NULL, movedTF = NULL, quiet = FALSE,
                              verbose = FALSE){

  
  return_if_null(files)
  moved <- basename(files[movedTF])
  not_moved <- basename(files[!movedTF])
  n_moved <- length(moved)
  n_not_moved <- length(not_moved)
  msg1 <- NULL
  msg2 <- NULL
  if(n_moved > 0){
    msg1 <- c(msg1, "moved:", moved)
    msg2 <- paste0(msg2, n_moved, " fits files moved")
  }
  if(n_not_moved > 0){
    msg1 <- c(msg1, "not moved: ", not_moved)
    if(n_moved > 0){
      msg2 <- paste(msg2, ", ")
    }
    msg2 <- paste0(msg2, n_moved, " fits files not moved")
  }
  if(!verbose){   
    msg1 <- NULL 
  }
  
  messageq(msg1, "   **", msg2, "**", quiet = quiet)
}

