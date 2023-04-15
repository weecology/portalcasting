#' @title Facilitate Adding Datasets to a Directory
#'
#' @description Create a new dataset's controls. Using the `dataset_controls_template` file as a basis for the `list` and leveraging element-specific functions to fill in details. \cr
#'              Each of the specific `new_dataset_< >` functions wraps an [`update_list`] call starting with the [`dataset_controls_template`] as the main list and taking any named elements as inputs via `...`.
#'
#' @details Having been created using [`new_dataset_controls`], the new dataset's controls can either be added to the directory at directory creation (via [`setup_dir`] or related [`setup_<>`][setup_dir] functions) or update (via [`update_dir`]) steps or via [`add_new_dataset`].
#'
#' @param ... Named `list` of arguments passed to [`update_list`].
#'
#' @param fun `character` value of the generation function.
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param new_dataset_controls `list` of controls for any new datasets (not in the prefab datasets) listed in `datasets` that are to be added to the control list and file.
#'
#' @return `dataset_controls_template`: `list` of named dataset controls elements, many as `NULL`. \cr  
#'         `new_dataset_controls`: `list` of named dataset controls. \cr 
#'         `new_dataset_metadata`: `list` of named dataset metadata elements to the controls `list`. \cr 
#'         `new_dataset_fun`: `character` of dataset generation function. \cr 
#'         `new_dataset_args`: named `list` of argument elements to the generating function. \cr 
#'         `add_new_dataset`: dataset controls `list` for the new dataset, [`invisible`][base::invisible]-ly.
#'
#' @name new datasets
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "new_dataset_controls")
#'    setup_dir(main = main1)
#'
#'    dataset_controls_template( )
#'
#'    args <- new_dataset_args(name     = "newdata", 
#'                             filename = "rodents_newdata.csv")
#'
#'    new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
#'                                         args     = args)
#'
#'    added <- add_new_dataset(main                 = main1, 
#'                             new_dataset_controls = new_controls)
#'
#'    unlink(main1, recursive = TRUE)
#'  }
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

  dataset_controls <- read_datasets_controls(main = main)


  nexisting_datasets <- length(dataset_controls)
  dataset_controls[[nexisting_datasets + 1]] <- new_dataset_controls

  ndatasets                        <- length(dataset_controls)
  names(dataset_controls)[ndatasets] <- new_dataset_controls$metadata$name            

  write_yaml(x    = dataset_controls,
             file = file.path(main, settings$subdirectories$data, settings$files$datasets_controls))


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




