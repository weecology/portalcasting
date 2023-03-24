


#' @title Read and Write Model Control Lists
#'
#' @description Input/output functions for model control lists.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param new_model_controls \code{list} of controls for any new models (not in the prefab models) listed in \code{models} that are to be added to the control list and file.
#'
#' @param model_controls_list \code{list} of controls for the models. 
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or just the tidy messages.
#'
#' @return 
#'   \code{model_controls}: \code{list} of \code{models}' control \code{list}s, \code{\link[base]{invisible}}-ly. \cr 
#'   \code{read_model_controls}: \code{list} of all \code{models}' control \code{list}s, from the file defined in \code{\link{directory_settings}}, \code{\link[base]{invisible}}-ly. \cr 
#'   \code{write_model_controls}: \code{list} of \code{models}' control \code{list}s, \code{\link[base]{invisible}}-ly. \cr 
#'   \code{write_model_scripts}: \code{NULL}, \code{\link[base]{invisible}}-ly.
#' 
#' @name read and write model controls
#'
#' @export
#'
read_model_controls <- function (main     = ".",
                                 settings = directory_settings( )) {

  read_yaml(file = file.path(main, settings$subdirectories$models, settings$files$model_controls))

}


#' @rdname read-and-write-model-controls
#'
#' @export
#'
model_controls <- function (main     = ".",
                            models   = prefab_models( ),
                            settings = directory_settings( )) {

  read_model_controls(main     = main, 
                      settings = settings)[models]


}

#' @rdname read-and-write-model-controls
#'
#' @export
#'
write_model_controls <- function (main               = ".",
                                  new_model_controls = NULL,
                                  models             = prefab_models( ),
                                  settings           = directory_settings( ),
                                  quiet              = FALSE) {

  messageq(" Writing model controls ... ", quiet = quiet)

  model_controls <- c(prefab_model_controls( ), new_model_controls)[models]
  nmodels        <- length(model_controls)

  for (i in 1:nmodels) {

    if(!is.null(model_controls[[i]]$fit$model_file)) {

      model_controls[[i]]$fit$full_model_file <- paste0("'", file.path(main, settings$subdirectories$models, model_controls[[i]]$fit$model_file), "'")

    } else {

      model_controls[[i]]$fit$full_model_file <- NULL

    }

  }

  write_yaml(x    = model_controls,
             file = file.path(main, settings$subdirectories$models, settings$files$model_controls))

  messageq("  ... done. ", quiet = quiet)

  invisible(model_controls)

}



#' @rdname read-and-write-model-controls
#'
#' @export
#'
write_model_scripts <- function (model_controls_list = prefab_model_controls( ), 
                                 quiet               = FALSE, 
                                 verbose             = FALSE) {

  files  <- unlist(mapply(getElement, mapply(getElement, model_controls_list, "fit"), "model_file"))
  ffiles <- unlist(mapply(getElement, mapply(getElement, model_controls_list, "fit"), "full_model_file"))

  nfiles <- length(files)
  if (nfiles > 0) {

    messageq(" Writing model script files ... ", quiet = quiet)

    for (i in 1:nfiles) {

      messageq("   - ", names(files)[i], quiet = !verbose)

      to_path   <- eval(parse(text     = ffiles[i]))
      from_path <- system.file(...     = "extdata", 
                               ...     = files[i], 
                               package = "portalcasting")
      file.copy(from = from_path,
                to   = to_path)


    }

    messageq("  ... done. ", quiet = quiet)

  }

  invisible( )

}