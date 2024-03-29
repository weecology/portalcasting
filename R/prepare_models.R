#' @title Prepare Portalcasting Models
#'
#' @description Add model controls and scripts to the portalcasting directory and read in controls and model names.
#'
#' @param main `character` value of the name of the main component of the directory tree. 
#'
#' @param models `character` vector of name(s) of model(s) to include.
#'
#' @param new_models_controls `list` of controls for any new models (not in the prefab models) listed in `models` that are to be added to the control list and file.
#'
#' @param controls `list` of controls for the models. 
#'
#' @return `model_controls`: `list` of `models`' control `list`s, [`invisible`][base::invisible]-ly. \cr 
#'         `read_models_controls`: `list` of all `models`' control `list`s, from the file defined in [`directory_settings`], [`invisible`][base::invisible]-ly. \cr 
#'         `write_models_controls`: `list` of `models`' control `list`s, [`invisible`][base::invisible]-ly. \cr 
#'         `write_models_scripts`: `NULL`, [`invisible`][base::invisible]-ly.
#'
#' @name prepare models
#'
#' @aliases prep-models models
#'
#' @family content-prep
#'
#' @examples
#' \dontrun{
#'    main1 <- file.path(tempdir(), "models")
#'
#'    create_dir(main = main1)
#'    fill_resources(main = main1)
#'    fill_forecasts(main = main1)
#'    fill_fits(main = main1)
#'
#'    controls <- write_models_controls(main = main1)
#'    write_models_scripts(main     = main1, 
#'                       controls = controls)
#'
#'    unlink(main1, recursive = TRUE)
#' }
#'
NULL

#' @rdname prepare-models
#'
#' @export
#'
write_models_controls <- function (main                = ".",
                                   new_models_controls = NULL,
                                   models              = prefab_models( )) {

  settings <- read_directory_settings(main = main)

  messageq("Writing models controls ...", quiet = settings$quiet)

  models_controls <- c(prefab_models_controls( ), new_models_controls)[models]
  nmodels         <- length(models_controls)

  for (i in 1:nmodels) {

    if(!is.null(models_controls[[i]]$fit$model_file)) {

      models_controls[[i]]$fit$full_model_file <- paste0("'", file.path(main, settings$subdirectories$models, models_controls[[i]]$fit$model_file), "'")

    } else {

      models_controls[[i]]$fit$full_model_file <- NULL

    }

  }

  write_yaml(x    = models_controls,
             file = models_controls_path(main = main))

  messageq(" ... complete.\n", quiet = settings$quiet)

  invisible(models_controls)

}



#' @rdname prepare-models
#'
#' @export
#'
write_models_scripts <- function (main     = ".",
                                  controls = prefab_models_controls( )) {

  settings <- read_directory_settings(main = main)

  files  <- unlist(mapply(getElement, mapply(getElement, controls, "fit"), "model_file"))

  nfiles <- length(files)
  if (nfiles > 0) {

    messageq("Writing models script files ...", quiet = settings$quiet)

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

#' @name prepare-models
#'
#' @export
#'
read_models_controls <- function (main = ".") {

  settings <- read_directory_settings(main = main)
  read_yaml(file = file.path(main, settings$subdirectories$models, settings$files$models_controls))

}


#' @rdname prepare-models
#'
#' @export
#'
models_controls <- function (main     = ".",
                             models   = NULL) {

  read_models_controls(main = main)[models]

  models_c <- read_models_controls(main = main)
  models   <- ifnull(models, names(models_c))
  models_c[models]

}