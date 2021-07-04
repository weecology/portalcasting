#' @title Create, update, and read the directory configuration file
#' 
#' @description The configuration file is a special file within the 
#'              portalcasting directory that has its own set of functions. 
#'              \cr \cr
#'              \code{write_config} creates the file. It is (and should only 
#'              be) called from within \code{\link{create_dir}}, as
#'              it captures information about the compute environment used to
#'              instantiate the directory. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'              the directory tree. 
#'
#'
#' @return \code{write_directory_config} and \code{update_directory_config}
#'  both write out the \code{dir_config.yaml} file and return \code{NULL}. 
#'  \cr \cr
#'  \code{read_directory_config}: \code{list} of directory configurations. 
#'
#' @name directory_config
#'
NULL

#' @rdname directory_config
#'
#' @export
#'
write_config <- function (main = "."){


  subs <- c("casts", "fits", "models", "resources", "data")


  write_yaml(
   list(date                  = as.character(Sys.Date()),
        R_version             = sessionInfo()$R.version,
        portalcasting_version = as.character(packageVersion("portalcasting")),
        directory_tree        = list(main = main, subs = subs),
        downloads_versions    = NULL), 
   file = normalized_file_path(main, filename,
                                         mustWork = FALSE))


}
#' @title Save data out to a file and return it	
#'
#' @description Save inputted data out to a data file if requested and 
#'              return it to the console.
#'
#' @param dfl \code{data.frame} or YAML \code{list} to be written out.
#'
#' @param main \code{character} value of the name of the main component of
#'             the directory tree. 
#'
#' @param save \code{logical} indicator controlling if \code{dfl} should 
#'             be saved out.
#'
#' @param filename \code{character} name of the file for saving \code{dfl}.
#'
#' @param overwrite \code{logical} indicator of if the file should be
#'                  overwritten if it exists.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{dfl} as input.
#'
#' @export
#'
write_data <- function (dfl = NULL, 
                        main = ".", 
                        save = TRUE, 
                        filename = NULL, 
                        overwrite = TRUE, 
                        quiet = FALSE){

  return_if_null(dfl)
  return_if_null(filename)

  save_it <- FALSE

  if (save) {

    fext <- file_ext(filename)
    full_path <- file.path(main = main, sub = "data", files = filename)
    f_exists <- file.exists(full_path)

    if (f_exists) {

      if (overwrite) {

        save_it <- TRUE
        msg <- paste0("    **", filename, 
                      " exists and overwrite = TRUE; file saved**")

      } else {

        msg <- paste0("    **", filename, 
                      " exists and overwrite = FALSE; not saved***") 

      }

    } else {

      save_it <- TRUE
      msg <- paste0("    **", filename, " saved**")

    }

    messageq(msg, quiet = quiet)

    if (save_it) {

        if (fext == "csv") {

          write.csv(dfl, full_path, row.names = FALSE)

      } else if (fext == "yaml") {

          write_yaml(x = dfl, file = full_path)

      } else {

        stop("file type not supported", call. = FALSE)

      }

    }
 
  }

  dfl

}

  
