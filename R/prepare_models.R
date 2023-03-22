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
#' @return \code{list} of \code{models}' control \code{list}s, \code{\link[base]{invisible}}-ly for \code{write_model_controls}.
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

  model_controls <- prefab_model_controls()

  nmodels     <- length(model_controls)
  nnew_models <- length(new_model_controls)

  if (nnew_models > 0) {

    for (i in 1:nnew_models) {

      model_controls <- update_list(model_controls, 
                                    x = new_model_controls[[i]])

      names(model_controls)[nmodels + i] <- names(new_model_controls)[i]

    }

  }

  for (i in 1:length(model_controls)) {

    if(!is.null(model_controls[[i]]$fit$model_file)) {
      fpath <- paste0("'", file.path(main, settings$subdirectories$models, model_controls[[i]]$fit$model_file), "'")
      model_controls[[i]]$fit$full_model_file <- fpath

    } else {

      model_controls[[i]]$fit$full_model_file <- NULL

    }

  }

  write_yaml(x    = model_controls,
             file = file.path(main, settings$subdirectories$models, settings$files$model_controls))

  invisible(model_controls)

}
