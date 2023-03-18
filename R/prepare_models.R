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

  write_yaml(x    = model_controls,
             file = file.path(main, settings$subdirectories$models, settings$files$model_controls))

  invisible(model_controls)

}






#' @title Write Model Function Script into Directory
#'
#' @description Writes a model's function as a script into the defined directory for use in forecasting. \cr \cr 
#'              \code{model} can be input as a \code{character} string, symbol (backquoted name), or \code{function}, as \code{\link{match.fun}}
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param model \code{character} name of a model function, the \code{function} itself, or its symbol (backquoted name).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @param datasets \code{character} vector of dataset names for the model. 
#'
#' @return \code{write_model} \code{\link{write}}s the model script out and returns \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
write_model <- function (main     = ".", 
                         model    = NULL, 
                         settings = directory_settings( ), 
                         quiet    = FALSE, 
                         verbose  = FALSE) {


  return_if_null(model)
  
  control_model <- model_controls(main     = main,
                                  models   = model,
                                  settings = settings)

  model_path    <- file.path(main, settings$subdirectories$models, paste0(model, ".R"))

  model_script <- 1

  if (file.exists(model_path) ) {

    messageq("  - Updating ", model, quiet = quiet)


  } else {

    messageq("  - Adding ", model, quiet = quiet)

  } 

  write(x    = model_script, 
        file = model_path)

  invisible( )

}



