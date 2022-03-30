

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
#' @return \code{write_mode} \code{\link{write}}s the model script out
#'  and returns \code{NULL}. \cr \cr
#'  \code{model_template}: \code{character}-valued text for a model script 
#'  to be housed in the model directory. \cr \cr
#'  \code{control_list_arg}: \code{character}-valued text for part of a 
#'  model script. \cr \cr
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
  datasets        <- control_model$datasets

  if (is.null(datasets)) {

    messageq("   ~datasets = NULL for ", model, "\n    **assuming datasets = prefab_rodent_datasets(interpolate = FALSE)**", quiet = quiet)
    datasets <- prefab_rodent_datasets(interpolate = FALSE)

  }

  model_file <- paste0(model, ".R")
  mod_path   <- file_path(main = main, sub = "models", files = model_file)


  mod_template <- model_template(main     = ".", 
                                 model    = model, 
                                 datasets = prefab_rodent_datasets(interpolate = FALSE),
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

}

#' @rdname write_model
#'
#' @export
#'
model_template <- function (main     = ".", 
                            model    = NULL, 
                            datasets = prefab_rodent_datasets(interpolate = FALSE),
                            settings = directory_settings(), 
                            quiet    = FALSE, 
                            verbose  = FALSE) {

  return_if_null(model)

  control_model   <- tryCatch(prefab_model_controls()[[model]],
                              error = function(x){NULL})

  datasets        <- control_model$datasets
  nds <- length(datasets)
  
  return_if_null(datasets)


  main_arg    <- paste0(', main = "', main, '"')
  quiet_arg   <- paste0(', quiet = ', quiet)
  verbose_arg <- paste0(', verbose = ', verbose)
  ds_args     <- paste0('dataset = "', datasets, '"')

 
  settings_arg <- paste0(', settings = directory_settings()')

  # need to allow generalization here!
  

  out <- NULL
  for(i in 1:nds){

    resp <- paste0('cast_', datasets[i])

    model_args <- paste0(ds_args[i], main_arg, settings_arg, quiet_arg, verbose_arg)

    model_fun  <- paste0(model, '(', model_args, ');')
    model_line <- paste0(resp, ' <- ', model_fun)
    save_args  <- paste0(resp, main_arg, quiet_arg)
    save_fun   <- paste0('save_cast_output(', save_args, ');')
    save_line  <- save_fun
    newout     <- c(model_line, save_line)
    out        <- c(out, newout)

  }
  out

}




#' @rdname write_model
#'
#' @export
#'
control_list_arg <- function(control_list = NULL, list_function = NULL,
                             arg_checks = TRUE){                             
  return_if_null(control_list)
  return_if_null(list_function)

  list_name <- paste(strsplit(list_function, "_")[[1]][2:1], collapse = "_")
  nvals <- length(control_list)
  val_values <- rep(NA, nvals)
  for(i in 1:nvals){
    val_name <- names(control_list)[i]
    val_value <- control_list[[i]]
    formal_value <- formals(eval(parse(text = list_function)))[[val_name]]

    if(!identical(val_value, formal_value)){
      if(is.character(val_value)){
        val_value <- paste0('"', val_value, '"')
      }
      if(is.null(val_value)){
        val_value <- "NULL"
      }
      val_values[i] <- val_value
    }
  }
  update_values <- which(is.na(val_values) == FALSE)
  nupdate_values <- length(update_values)
  val_texts <- NULL
  if(nupdate_values > 0){
    val_text <- rep(NA, nupdate_values)
    update_val_names <- names(control_list)[update_values]
    update_val_values <- val_values[update_values]
    for(i in 1:nupdate_values){
      val_text[i] <- paste0(update_val_names[i], ' = ', update_val_values[i])
    }
    val_texts <- paste0(val_text, collapse = ', ')
  }
  paste0(', ', list_name, ' = ', list_function, '(', val_texts, ')')
}


#' @title Create a control list for a model
#'
#' @description Provides a ready-to-use template for the 
#'  controls \code{list} used to define the specific arguments for a user's
#'  novel model, setting the formal arguments to the basic default 
#'  values.
#
#' @param name \code{character} value of the name of the model.
#'
#' @param data_sets \code{character} vector of the rodent data set names
#'  that the model is applied to. Defaults to all non-interpolated data sets.
#'
#' @param covariatesTF \code{logical} indicator for if the model requires 
#'  covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NULL} if \code{covariatesTF} is \code{FALSE}.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return Named \code{list} of model's script-generating controls, 
#'  for input as part of \code{controls_m} in \code{\link{write_model}}.
#'
#' @export
#'
model_control <- function(name = "model", 
                          data_sets = prefab_data_sets(interpolate = FALSE),
                          covariatesTF = FALSE, lag = NA, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  list(name = name, data_sets = data_sets, covariatesTF = covariatesTF, 
      lag = lag)
}

#' @title Verify that models requested to cast with exist
#'
#' @description Verify that models requested have scripts in the models 
#'   subdirectory. 
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param models \code{character} vector of the names of models to verify. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{NULL} with messaging. 
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   fill_models()
#'   verify_models()
#'  }
#'
#' @export
#'
verify_models <- function(main = ".", models = prefab_models(), 
                          quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  messageq("Checking model availability", quiet = quiet)
  model_dir <- sub_path(main = main, subs = "models", arg_checks = arg_checks)
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist", call. = FALSE)
  }
  available <- list.files(model_dir)
  if (models[1] != "all"){
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      msg <- paste0("Requested model(s) ", missmod, " not in directory \n")
      stop(msg, call. = FALSE)
    }
  }
  messageq(" *All requested models available*", quiet = quiet)
  messageq(message_break(), quiet = quiet)
  invisible(NULL)
}


#' @title Update models based on user input controls
#'
#' @description Update model scripts based on the user-defined model control
#'  inputs. This allows users to define their own models or re-define prefab
#'  models within the \code{\link{portalcast}} pipeline.
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s), used to 
#'  restrict the models to update from \code{controls_model}. The default
#'  (\code{NULL}) translates to all models in the \code{controls_model} list,
#'  which is recommended for general usage and is enforced within the main
#'  \code{\link{portalcast}} pipeline. 
#'
#' @param controls_model Controls for models not in the prefab set or for 
#'  overriding those in the prefab set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include four 
#'  elements: 
#'  \itemize{
#'   \item \code{name}: a \code{character} value of the model name.
#'   \item \code{data_sets}: a \code{character} vector of the data set names
#'    that the model is applied to. 
#'   \item \code{covariatesTF}: a \code{logical} indicator of if the 
#'    model needs covariates.
#'   \item \code{lag}: an \code{integer}-conformable value of the lag to use 
#'    with the covariates or \code{NA} if \code{covariatesTF = FALSE}.
#'  } 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out
#'  all of the information or not (and thus just the tidy messages). 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param bline \code{logical} indicator if horizontal break lines should be
#'  included in messaging.
#'
#' @param update_prefab_models \code{logical} indicator if all of the models'
#'  scripts should be updated, even if they do not have an explicit change
#'  to their model options via \code{controls_model}. Default is
#'  \code{FALSE}, which leads to only the models in \code{controls_model}
#'  having their scripts re-written. Switching to \code{TRUE} results in the
#'  models listed in \code{models} having their scripts re-written. \cr \cr
#'  This is particularly helpful when one is changing the global (with respect
#'  to the models) options \code{main}, \code{quiet}, \code{verbose}, or
#'  \code{control_files}.
#'
#' @return \code{NULL}. 
#'
#' @examples
#'  \donttest{
#'   setup_dir()
#'   cm <- model_control(name = "AutoArima", data_sets = "all")
#'   update_models(controls_model = cm)
#'  }
#'
#' @export
#'
update_models <- function(main = ".", models = NULL,
                          controls_model = NULL, update_prefab_models = FALSE, 
                          control_files = files_control(), bline = FALSE,
                          quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  if(list_depth(controls_model) == 1){
    controls_model <- list(controls_model)
    names(controls_model) <- controls_model[[1]]$name
  }
  if(update_prefab_models){
    controls_model <- model_controls(models = models, 
                                     controls_model = controls_model, 
                                     quiet = quiet, arg_checks = arg_checks)
  } else{
    models <- NULL
  }
  return_if_null(c(controls_model, models))
  models <- unique(c(models, names(controls_model)))
  messageq("Updating model scripts", quiet = quiet)
  nmodels <- length(models)
  for(i in 1:nmodels){
    write_model(main = main, quiet = quiet, verbose = verbose, 
                control_files = control_files, 
                control_model = controls_model[[models[i]]], 
                arg_checks = arg_checks)
  }
  messageq(message_break(), quiet = quiet)
  invisible(NULL)
}


#' @title Create a covariate model list
#'
#' @description Convenience function for creating covariate model 
#'  \code{list}s.
#'
#' @param model \code{character} name for covariate models. Currently only 
#'  \code{"pevGARCH"} is supported.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @return \code{list} of covariate model structures.
#'
#' @examples
#'  covariate_models()
#'
#' @export
#'
covariate_models <- function(model = "pevGARCH", arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  out <- NULL
  if (model == "pevGARCH"){
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
