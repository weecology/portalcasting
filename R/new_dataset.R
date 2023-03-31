#' @title Facilitate Adding Datasets to a Directory
#'
#' @description Create a new dataset's controls. Using the \code{dataset_controls_template} file as a basis for the \code{list} and leveraging element-specific functions to fill in details. \cr
#'              Each of the specific \code{new_dataset_< >} functions wraps an \code{\link{update_list}} call starting with the \code{\link{dataset_controls_template}} as the main list and taking any named elements as inputs via \code{...}.
#'
#' @details Having been created using \code{link{new_dataset_controls}}, the new dataset's controls can either be added to the directory at directory creation (via \code{\link{setup_dir}} or related \code{setup_<>} functions) or update (via \code{\link{update_dir}}) steps or via \code{link{add_new_dataset}}.
#'
#' @param ... Named \code{list} of arguments passed to \code{\link{update_list}}.
#'
#' @param fun \code{character} value of the generation function.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param new_dataset_controls \code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets} that are to be added to the control list and file.
#'
#' @return \code{dataset_controls_template}: \code{list} of named dataset controls elements, many as \code{NULL}. \cr 
#'         \code{new_dataset_controls}: \code{list} of named dataset controls. \cr 
#'         \code{new_dataset_metadata}: \code{list} of named dataset metadata elements to the controls \code{list}. \cr 
#'         \code{new_dataset_fun}: \code{character} of dataset generation function. \cr 
#'         \code{new_dataset_args}: named \code{list} of argument elements to the generating function. \cr 
#'         \code{add_new_dataset}: dataset controls \code{list} for the new dataset, \code{\link[base]{invisible}}-ly.
#'
#' @name new datasets
#'
NULL

#' @rdname new-datasets
#'
#' @export
#'
dataset_controls_template <- function( ) {

  dataset_controls_template_file <- system.file(...     = "extdata", 
                                                ...     = "dataset_controls_template.yaml", 
                                                package = "portalcasting")

  read_yaml(file = dataset_controls_template_file)[["dataset_name"]]

}


#' @rdname new-datasets
#'
#' @export
#'
add_new_dataset <- function (main                 = "." ,
                             new_dataset_controls = dataset_controls_template()) {

  settings <- read_directory_settings(main = main)

  messageq("Updating dataset controls ...", quiet = settings$quiet)

  dataset_controls <- read_dataset_controls(main = main)


  nexisting_datasets <- length(dataset_controls)
  dataset_controls[[nexisting_datasets + 1]] <- new_dataset_controls

  ndatasets                        <- length(dataset_controls)
  names(dataset_controls)[ndatasets] <- new_dataset_controls$metadata$name            

  write_yaml(x    = dataset_controls,
             file = file.path(main, settings$subdirectories$data, settings$files$dataset_controls))


  messageq(" ... complete.\n", quiet = settings$quiet)


  invisible(new_dataset_controls)

}

#' @rdname new-datasets
#'
#' @export
#'
new_dataset_controls <- function (...) {

  update_list(list = dataset_controls_template( ),
              ...  = ...)

}

#' @rdname new-datasets
#'
#' @export
#'
new_dataset_metadata <- function (...) {

  update_list(list = dataset_controls_template( )$metadata,
              ...  = ...)

}

#' @rdname new-datasets
#'
#' @export
#'
new_dataset_fun <- function (fun = dataset_controls_template( )$fun) {

  fun

}

#' @rdname new-datasets
#'
#' @export
#'
new_dataset_args <- function (...) {

  update_list(list = dataset_controls_template( )$args,
              ...  = ...)

}




