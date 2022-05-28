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
                                 settings = directory_settings()) {

  read_yaml(file.path(main, settings$subs$models, settings$files$model_controls))

}


#' @rdname read-and-write-model-controls
#'
#' @export
#'
model_controls <- function (main     = ".",
                            models   = prefab_models(),
                            settings = directory_settings()) {

  read_model_controls(main     = main, 
                      settings = settings)[[models]]

}

#' @rdname read-and-write-model-controls
#'
#' @export
#'
write_model_controls <- function (main               = ".",
                                  new_model_controls = NULL,
                                  models             = prefab_models(),
                                  settings           = directory_settings(),
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
             file = file.path(main, settings$subs$models, settings$files$model_controls))

  invisible(model_controls)

}






#' @title Write Model Function Script into Directory
#'
#' @description Writes a model's function as a script into the defined directory for use in forecasting. \cr \cr \code{model} can be input as a \code{character} string, symbol (backquoted name), or \code{function}, as \code{\link{match.fun}}
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param model \code{character} name of a model function, the \code{function} itself, or its symbol (backquoted name).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @param datasets \code{character} vector of dataset names for the model. 
#'
#' @return \code{write_mode} \code{\link{write}}s the model script out and returns \code{NULL}, \code{\link[base]{invisible}}-ly.. \cr \cr
#'  \code{model_template}: \code{character}-valued text for a model script to be housed in the model directory. \cr \cr
#'  \code{control_list_arg}: \code{character}-valued text for part of a model script. \cr \cr
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   write_model("AutoArima")
#'   model_template()
#'   control_list_arg(runjags_control(nchains = 3), "runjags_control")
#'  }
#'
#' @export
#'
write_model <- function (main     = ".", 
                         model    = NULL, 
                         settings = directory_settings(), 
                         quiet    = FALSE, 
                         verbose  = FALSE) {


  return_if_null(model)
  
  control_model   <- tryCatch(prefab_model_controls()[[model]],
                              error = function(x){NULL})
  datasets <- control_model$datasets

  if (is.null(datasets)) {

    messageq("   ~datasets = NULL for ", model, quiet = quiet)
    datasets <- prefab_datasets()

  }

  model_file <- paste0(model, ".R")
  mod_path   <- file.path(main, settings$subs$models, model_file)


  mod_template <- model_template(main     = main, 
                                 model    = model, 
                                 datasets = prefab_datasets(),
                                 settings = directory_settings(), 
                                 quiet    = FALSE, 
                                 verbose  = FALSE)

  if (file.exists(mod_path) & settings$overwrite) {

    write(mod_template, mod_path)
    messageq("  -", ifelse(verbose, "Updating ", ""), model, quiet = quiet)


  } else if (!file.exists(mod_path)) {

    write(mod_template, mod_path)
    messageq("  -", ifelse(verbose, "Adding ", ""), model, quiet = quiet)

  } 

  invisible()

}

#' @rdname write_model
#'
#' @export
#'
model_template <- function (main     = ".", 
                            model    = NULL, 
                            datasets = NULL,
                            settings = directory_settings(), 
                            quiet    = FALSE, 
                            verbose  = FALSE) {

  return_if_null(model)

  control_model   <- tryCatch(prefab_model_controls()[[model]],
                              error = function(x){NULL})

  datasets <- control_model$datasets
  nds             <- length(datasets)
  
  return_if_null(datasets)


  main_arg     <- paste0(', main = "', main, '"')
  quiet_arg    <- paste0(', quiet = ', quiet)
  verbose_arg  <- paste0(', verbose = ', verbose)
  ds_args      <- paste0('dataset = "', datasets, '"')
  settings_arg <- paste0(', settings = directory_settings()')

  additional_args <- NULL

  nadditional_args <- length(control_model$args)

  if (nadditional_args > 0) {

    for (i in 1:nadditional_args) {

      additional_args <- paste0(additional_args, ", ", names(control_model$args)[i], " = ", control_model$args[i])

    }

  }
  

  out <- NULL
  for(i in 1:nds){

    resp <- paste0('cast_', datasets[i])

    model_args <- paste0(ds_args[i], main_arg, settings_arg, quiet_arg, verbose_arg, additional_args)

    model_fun  <- paste0(model, '(', model_args, ');')
    model_line <- paste0(resp, ' <- ', model_fun)
    save_args  <- paste0(resp, main_arg, settings_arg, quiet_arg)
    save_fun   <- paste0('save_cast_output(', save_args, ');')
    save_line  <- save_fun
    newout     <- c(model_line, save_line)
    out        <- c(out, newout)

  }

  out

}


#' @title Create a covariate model list
#'
#' @description Convenience function for creating covariate model \code{list}s.
#'
#' @param model \code{character} name for covariate models. Currently only \code{"pevGARCH"} is supported.
#'
#' @return \code{list} of covariate model structures.
#'
#' @examples
#'  covariate_models()
#'
#' @export
#'
covariate_models <- function (model = "pevGARCH") {

  out <- NULL
  if (model == "pevGARCH") {

    out <- list(c("maxtemp", "meantemp", "precipitation", "ndvi"),
                c("maxtemp", "mintemp", "precipitation", "ndvi"),
                c("mintemp", "maxtemp", "meantemp", "precipitation"),
                c("precipitation", "ndvi"),
                c("mintemp", "ndvi"),
                c("mintemp"),
                c("maxtemp"),
                c("meantemp"),
                c("precipitation"),
                c("ndvi"),
                c(NULL))

  }

  out

}
