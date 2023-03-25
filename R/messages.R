#' @title Functions for Message Generation 
#'
#' @description Create messages for use in the portalcasting pipeline. 
#'   \code{messageq}: Optionally generate a message based on a logical input. Uses a wrapper on \code{\link[base]{message}} that, given the input to \code{quiet}, generates the message(s) in \code{...} or not. \cr \cr
#'   \code{break_line}: Creates a horizontal line of characters ending with a newline call for messages. \cr \cr
#'   \code{break_lines}: Creates a set of horizontal line of characters ending with a newline call for messages. \cr \cr
#'   \code{castle}: Creates a text drawing of a sandcastle of characters for messages. \cr \cr 
#'   Many message functions wrap around \code{messageq} to produce specific messages, see \code{Details}.
#'
#' @details The following specific message functions exist: \cr
#'   \code{sandbox_creation_message}, \code{evaluation_message}, \code{completion_message}, \code{file_saved_message}, \code{file_updated_message}, 
#'   \code{downloading_message}, \code{not_updated_message}, \code{directory_creation_start_message}, \code{directory_creation_complete_message},
#'   \code{writing_model_controls_message}, \code{writing_model_scripts_message}, \code{model_message}, \code{data_message}, \code{subdata_message}, 
#'   \code{directory_filling_message}, \code{directory_filling_completed_message}, \code{data_filling_message}, \code{data_removing_message}, 
#'   \code{data_adding_message}, \code{resources_filling_message}, \code{moving_message}, \code{files_moved_message}, \code{files_moved_or_not_message},
#'   \code{files_located_message}, \code{file_creating_message}, \code{cast_message}, \code{portalcasting_message}, \code{model_failed_message},
#'   \code{model_succeeded_message}, \code{portalcasting_started_message}, \code{portalcasting_completed_message}.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
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
#' @param model \code{character} name of a model.
#'
#' @param dataset \code{character} dataset name.
#'
#' @param species \code{character} species name.
#'
#' @param data_type \code{character} of the type of data (e.g., \code{"newmoons"}, \code{"rodents"}, \code{"metadata"}, \code{"covariates"}) .
#'
#' @param file_type \code{character} of the type of file (e.g., \code{"casts"}, \code{"fits"}) 
#'
#' @param reps \code{integer}-conformable value for number of times \code{char} is replicated.
#' 
#' @param nlines \code{integer}-conformable value for the number of \code{\link{break_line}}s to include. Defaults to \code{2} lines.
#'
#' @param file,files \code{character} filename of the file saved. If plural (\code{files}) can be multiple, otherwise must only be one file.
#'
#' @param nfiles \code{integer}-conformable value for the number of files.
#'
#' @param resource \code{character} of the resource's name.
#'
#' @param version \code{character} of the version of the resource.
#'
#' @param moved \code{logical} vector indicating if the file(s) were moved.
#'
#' @return 
#'   \code{messageq}: A message is given, and \code{NULL} returned, \code{\link[base]{invisible}}-ly. \cr
#'   \code{break_line}: The \code{character} of the line to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   \code{break_lines}: The \code{character} of the lines to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   \code{castle}: The \code{character} of the sandcastle to be passed to \code{\link[base]{message}} or \code{\link{messageq}}. \cr 
#'   For specific message functions, a message is given, and \code{NULL} returned, \code{\link[base]{invisible}}-ly. 
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


#' @rdname messages
#'
#' @export
#'
sandbox_creation_message <- function (main  = ".",
                                      quiet = FALSE) {

  messageq(castle(), "Sandbox directory successfully set up at \n\n  ", normalizePath(file.path(main = main)), "\n\nHappy model building!", quiet = quiet)

}


#' @rdname messages
#'
#' @export
#'
directory_creation_start_message <- function (main  = ".",
                                              quiet = FALSE) {

  core_package_version <- package_version_finder("setup_dir")

  messageq(break_lines( ), "This is ", core_package_version[["package"]], " v", core_package_version[["version"]], "\n  ", format(Sys.time(), "%x %T %Z"), "\n", break_lines( ), "Establishing directory at\n ", normalizePath(file.path(main = main), mustWork = FALSE), "\n", break_lines( ), quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
directory_creation_complete_message <- function (quiet = FALSE) {

  messageq(break_lines( ), "Directory successfully instantiated\n", break_lines( ), quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
not_updated_message <- function (version = NULL, 
                                 quiet   = FALSE) {

  return_if_null(x = version)
  messageq("Existing local version is up-to-date with remote version (", version, ") requested and `overwrite` is FALSE, download is skipped", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
downloading_message <- function (resource = NULL, 
                                 version  = NULL, 
                                 quiet    = FALSE) {

  messageq("Downloading ", resource, "version `", version, "` ...", quiet = quiet)

}


#' @rdname messages
#'
#' @export
#'
file_updated_message <- function (file  = NULL, 
                                  quiet = FALSE) {

  return_if_null(x = file)
  messageq("    **", file, " updated**", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
file_saved_message <- function (file  = NULL, 
                                quiet = FALSE) {

  return_if_null(x = file)
  messageq("    **", file, " saved**", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
completion_message <- function (quiet = FALSE) {

  messageq(" ... complete.\n", quiet = quiet)

}



#' @rdname messages
#'
#' @export
#'
evaluation_message <- function (quiet = FALSE) {

  messageq("Evaluating casts ...\n", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
writing_model_controls_message <- function (quiet = FALSE) {

  messageq("Writing model controls ...\n", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
writing_model_scripts_message <- function (quiet = FALSE) {

  messageq("Writing model script files ...\n", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
model_message <- function (model = NULL,
                           quiet = FALSE) {

  return_if_null(x = model)
  messageq("   - ", model, quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
data_type_message <- function (data_type = NULL,
                               quiet     = FALSE) {

  return_if_null(x = data)
  messageq("  - ", data_type, quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
dataset_message <- function (dataset = NULL,
                             quiet   = FALSE) {

  return_if_null(x = dataset)
  messageq("    - ", dataset, quiet = quiet)

}


#' @rdname messages
#'
#' @export
#'
directory_filling_message <- function (quiet = FALSE) {

  messageq("Filling directory with content: \n", quiet = quiet)

}


#' @rdname messages
#'
#' @export
#'
directory_filling_completed_message <- function (quiet = FALSE) {

  messageq("\nDirectory filling complete.", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
model_failed_message <- function (quiet = FALSE) {

  messageq("    |------| failed |------|", quiet = quiet) 

}

#' @rdname messages
#'
#' @export
#'
model_succeeded_message <- function (quiet = FALSE) {

 messageq("    |++++| successful |++++|", quiet = quiet )    

}


#' @rdname messages
#'
#' @export
#'
portalcasting_started_message <- function (quiet = FALSE) {

  messageq(break_line( ), "Forecasting models...\n", 
           break_line( ), "This is portalcasting v", packageDescription("portalcasting", fields = "Version"), "\n", 
           break_line( ), quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
portalcasting_completed_message <- function (quiet = FALSE) {

  messageq(break_line( ), "...forecasting complete.\n", break_line( ), quiet = quiet)

}


#' @rdname messages
#'
#' @export
#'
data_adding_message <- function (quiet = FALSE) {

  messageq("  ... adding data files ... ", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
moving_message <- function (quiet = FALSE) {

  messageq("  ... moving ... ", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
files_moved_message <- function (nfiles = NULL,
                                 quiet  = FALSE) {

  return_if_null(x = nfiles)
  messageq(paste0("  ... ", nfiles, " files moved. "), quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
files_moved_or_not_message <- function (files  = NULL,
                                        copied = NULL,
                                        quiet  = FALSE) {

  return_if_null(x = nfiles)
  return_if_null(x = copied)
  messageq(paste(ifelse(sum(copied) > 0, 
                   paste("  moved:", files[copied], collapse = "\n   "),
                   ""),
                 ifelse(sum(!copied) > 0, 
                   paste("  not moved:", files[!copied], collapse = "\n   "),
                   ""),
                 collapse = "\n"),
           quiet = quiet)
}



#' @rdname messages
#'
#' @export
#'
file_creating_message <- function (file  = NULL,
                                   quiet = FALSE) {

  messageq("  **creating ", file," file**", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
cast_message <- function (model   = NULL,
                          dataset = NULL,
                          species = NULL,
                          quiet   = FALSE) {

  return_if_null(x = dataset)
  return_if_null(x = species)
  return_if_null(x = model)

  messageq("  - ", model, " for ", dataset, " ", species, quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
files_located_message <- function (nfiles    = NULL,
                                   file_type = NULL,
                                   quiet     = FALSE) {

  return_if_null(x = nfiles)
  messageq(paste0(" Located ", nfiles, " ", file_type, " file(s) in resources to be moved to directory ..."), quiet = quiet)

}


#' @rdname messages
#'
#' @export
#'
data_filling_message <- function (quiet = FALSE) {

  messageq(" Preparing data files ... ", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
resources_filling_message <- function (quiet = FALSE) {

  messageq("Downloading resources ... ", quiet = quiet)

}

#' @rdname messages
#'
#' @export
#'
data_removing_message <- function (quiet = FALSE) {

  messageq("  ... removing existing data files ... ", quiet = quiet)

}