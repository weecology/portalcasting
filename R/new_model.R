#' @title Facilitate Adding Models to a Directory
#'
#' @description Create a new model's controls. Using the \code{model_controls_template} file as a basis for the \code{list} and leveraging element-specific functions to fill in details. \cr
#'              Each of the specific \code{new_model_< >} functions wraps an \code{\link{update_list}} call starting with the \code{\link{model_controls_template}} as the main list and taking any named elements as inputs via \code{...}.
#'
#' @details Having been created using \code{\link{new_model_controls}}, the new model's controls can either be added to the directory at directory creation (via \code{\link{setup_dir}} or related \code{setup_<>} functions) or update (via \code{\link{update_dir}}) steps or via \code{\link{add_new_model}}.
#'
#' @param ... Named \code{list} of arguments passed to \code{\link{update_list}}.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param new_model_controls \code{list} of controls for any new models (not in the prefab models) listed in \code{models} that are to be added to the control list and file.
#'
#' @return \code{model_controls_template}: \code{list} of named model controls elements, many as \code{NULL}. \cr 
#'         \code{new_model_controls}: \code{list} of named model controls. \cr 
#'         \code{new_model_metadata}: \code{list} of named model metadata elements to the controls \code{list}. \cr 
#'         \code{new_model_fit}: \code{list} of named model fit function and argument elements to the controls \code{list}. \cr 
#'         \code{new_model_cast}: \code{list} of named forecast function and argument elements to the controls \code{list}. \cr 
#'         \code{new_model_interpolate}: \code{list} of named interpolation requirements elements to the controls \code{list}. \cr 
#'         \code{new_model_datasets}: \code{list} of named dataset elements to the controls \code{list}. \cr 
#'         \code{new_model_response}: \code{list} of named response data description elements to the controls \code{list}. \cr
#'         \code{add_new_model}: model controls \code{list} for the new model, \code{\link[base]{invisible}}-ly.
#'
#' @name new models
#'
NULL

#' @rdname new-models
#'
#' @export
#'
model_controls_template <- function( ) {

  model_controls_template_file <- system.file(...     = "extdata", 
                                              ...     = "model_controls_template.yaml", 
                                              package = "portalcasting")

  read_yaml(file = model_controls_template_file)[["model_name"]]

}

#' @rdname new-models
#'
#' @export
#'
add_new_model <- function (main               = "." ,
                           new_model_controls = model_controls_template()) {

  settings <- read_directory_settings(main = main)

  messageq("Updating model controls ...", quiet = settings$quiet)

  model_controls <- read_model_controls(main = main)

  if(!is.null(new_model_controls$fit$model_file)) {

    new_model_controls$fit$full_model_file <- paste0("'", file.path(main, settings$subdirectories$models, new_model_controls$fit$model_file), "'")

    write_model_scripts(main     = main,
                        controls = list(new_model_controls))

  } else {

    new_model_controls$fit$full_model_file <- NULL

  }

  nexisting_models <- length(model_controls)
  model_controls[[nexisting_models + 1]] <- new_model_controls

  nmodels                        <- length(model_controls)
  names(model_controls)[nmodels] <- new_model_controls$metadata$name            

  write_yaml(x    = model_controls,
             file = file.path(main, settings$subdirectories$models, settings$files$model_controls))


  messageq(" ... complete.\n", quiet = settings$quiet)


  invisible(new_model_controls)

}

#' @rdname new-models
#'
#' @export
#'
new_model_controls <- function (...) {

  update_list(list = model_controls_template( ),
              ...  = ...)

}

#' @rdname new-models
#'
#' @export
#'
new_model_metadata <- function (...) {

  update_list(list = model_controls_template( )$metadata,
              ...  = ...)

}

#' @rdname new-models
#'
#' @export
#'
new_model_fit <- function (...) {

  update_list(list = model_controls_template( )$fit,
              ...  = ...)

}

#' @rdname new-models
#'
#' @export
#'
new_model_cast <- function (...) {

  update_list(list = model_controls_template( )$cast,
              ...  = ...)

}

#' @rdname new-models
#'
#' @export
#'
new_model_interpolate <- function (...) {

  update_list(list = model_controls_template( )$interpolate,
              ...  = ...)

}

#' @rdname new-models
#'
#' @export
#'
new_model_datasets <- function (...) {

  update_list(list = model_controls_template( )$datasets,
              ...  = ...)

}

#' @rdname new-models
#'
#' @export
#'
new_model_response <- function (...) {

  update_list(list = model_controls_template( )$response,
              ...  = ...)

}




