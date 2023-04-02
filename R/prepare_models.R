#' @title Read and Write Model Control Lists
#'
#' @description Input/output functions for model control lists, according to the[`directory_settings`]..
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param models `character` vector of name(s) of model(s) to include.
#'
#' @param new_model_controls `list` of controls for any new models (not in the prefab models) listed in `models` that are to be added to the control list and file.
#'
#' @param controls `list` of controls for the models. 
#'
#' @return 
#'   `model_controls`: `list` of `models`' control `list`s, [`base::invisible`]-ly. \cr 
#'   `read_model_controls`: `list` of all `models`' control `list`s, from the file defined in[`directory_settings`], [`base::invisible`]-ly. \cr 
#'   `write_model_controls`: `list` of `models`' control `list`s, [`base::invisible`]-ly. \cr 
#'   `write_model_scripts`: `NULL`, [`base::invisible`]-ly.
#' 
#' @name read and write model controls
#'
#' @export
#'
read_model_controls <- function (main = ".") {

  settings <- read_directory_settings(main = main)
  read_yaml(file = file.path(main, settings$subdirectories$models, settings$files$model_controls))

}


#' @rdname read-and-write-model-controls
#'
#' @export
#'
model_controls <- function (main     = ".",
                            models   = prefab_models( )) {

  read_model_controls(main = main)[models]

}

#' @rdname read-and-write-model-controls
#'
#' @export
#'
write_model_controls <- function (main               = ".",
                                  new_model_controls = NULL,
                                  models             = prefab_models( )) {

  settings <- read_directory_settings(main = main)

  messageq("Writing model controls ...", quiet = settings$quiet)

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

  messageq(" ... complete.\n", quiet = settings$quiet)

  invisible(model_controls)

}



#' @rdname read-and-write-model-controls
#'
#' @export
#'
write_model_scripts <- function (main     = ".",
                                 controls = prefab_model_controls( )) {

  settings <- read_directory_settings(main = main)

  files  <- unlist(mapply(getElement, mapply(getElement, controls, "fit"), "model_file"))

  nfiles <- length(files)
  if (nfiles > 0) {

    messageq("Writing model script files ...", quiet = settings$quiet)

    for (i in 1:nfiles) {

      messageq("   - ", names(files)[i], quiet = !settings$verbose)

      from_path <- system.file(...     = "extdata", 
                               ...     = files[i], 
                               package = "portalcasting")
      to_path   <- file.path(main, settings$subdirectories$models, files[i])
      file.copy(from      = from_path,
                to        = to_path, 
                overwrite = TRUE)
    }

    messageq(" ... complete.\n", quiet = settings$quiet)

  }

  invisible( )

}