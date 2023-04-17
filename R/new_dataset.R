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
#' @param models `character` vector of the names of the models that are to have their controls updated to include the new dataset.
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
#'                             new_dataset_controls = new_controls,
#'                             models               = "AutoArima")
#'  
#'    portalcast(main     = main1, 
#'               datasets = "newdata", 
#'               models   = "AutoArima")
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
                             new_dataset_controls = dataset_controls_template(),
                             models               = NULL) {

  return_if_null(x = models)

  settings <- read_directory_settings(main = main)

  messageq("Updating dataset controls ...", quiet = settings$quiet)

  datasets_controls <- read_datasets_controls(main = main)


  nexisting_datasets <- length(datasets_controls)
  datasets_controls[[nexisting_datasets + 1]] <- new_dataset_controls

  ndatasets                           <- length(datasets_controls)
  names(datasets_controls)[ndatasets] <- new_dataset_controls$metadata$name            

  write_yaml(x    = datasets_controls,
             file = file.path(main, settings$subdirectories$data, settings$files$datasets_controls))

  messageq(" ... complete.\n", quiet = settings$quiet)

  messageq("Adding dataset file ...", quiet = settings$quiet)

  do.call(what = new_dataset_controls$fun, 
          args = update_list(list = new_dataset_controls$args, 
                             main = main))
  
  messageq(" ... complete.\n", quiet = settings$quiet)

  messageq("Updating model controls ...", quiet = settings$quiet)

  nmodels <- length(models)
 
  species <- new_dataset_controls$args$species
  if (new_dataset_controls$args$total) {
    species <- c(species, "total")
  }

  models_controls <- read_models_controls(main = main)

  for (i in 1:nmodels) {

    model_controls <- models_controls[[models[i]]]

    model_controls$datasets <- c(model_controls$datasets, tempname = list(list(species = species)))

    names(model_controls$datasets)[length(model_controls$datasets)] <- new_dataset_controls$metadata$name

    models_controls[[models[i]]] <- model_controls

  }

  write_yaml(x    = models_controls, 
             file = file.path(main, settings$subdirectories$models, settings$files$models_controls))
  
  messageq(" ... complete.\n", quiet = settings$quiet)
 

  messageq("Updating metadata ...", quiet = settings$quiet)

  metadata <- read_metadata(main = main)

  metadata$datasets_controls <- datasets_controls

  write_yaml(x    = metadata, 
             file = file.path(main, settings$subdirectories$data, settings$files$metadata))

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




