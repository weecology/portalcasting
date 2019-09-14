prefab_model_controls <- function(){
  list(
    AutoArima = list(name = "AutoArima", 
                     data_sets = c("all", "controls"),
                     covariatesTF = FALSE, 
                     lag = NA), 
    ESSS = list(name = "ESSS", 
                data_sets = c("all_interp", "controls_interp"),
                covariatesTF = FALSE, 
                lag = NA), 
    NaiveArima = list(name = "NaiveArima", 
                      data_sets = c("all", "controls"),
                      covariatesTF = FALSE, 
                      lag = NA), 
    nbGARCH = list(name = "nbGARCH", 
                   data_sets = c("all_interp", "controls_interp"),
                   covariatesTF = FALSE, 
                   lag = NA), 
    nbsGARCH = list(name = "nbsGARCH", 
                    data_sets = c("all_interp", "controls_interp"),
                    covariatesTF = FALSE, 
                    lag = NA), 
    pevGARCH = list(name = "pevGARCH",  
                    data_sets = c("all_interp", "controls_interp"),
                    covariatesTF = TRUE, lag = 6)
  )
}


#' @title Produce the control lists for models
#' 
#' @description The models are written and managed using generalized functions
#'  that take a few arguments that vary among models, and so this function
#'  produces the appropriate \code{list} of \code{list}s of model
#'  script-writing control arguments. The specific \code{models} must be
#'  input by the user, with the default being \code{NULL}, which returns.
#'  \code{NULL}. The prefab models (\code{\link{prefab_models}}) have their
#'  control lists included already, but any user-created model needs to
#'  have controls provided. See \code{Details}. \cr 
#'  \strong{Users adding models to the prefab set should add their
#'  script-writing controls lists to the \code{prefab_controls} object in 
#'  the source code of the function.} \cr \cr
#'  \code{extract_min_lag} provides a simple function to extract the 
#'  minimum non-0 lag time used across models from the controls lists.
#'
#' @details Any model that is part of the \code{prefab} set 
#'  (\code{c("AutoArima", "NaiveArima", "ESSS", "nbGARCH", "nbsGARCH", 
#'  "pevGARCH")}) has its script-writing controls already included internally
#'  via the non-exported function \code{prefab_model_controls}. Users 
#'  only need to include controls for non-prefab \code{models}. \cr \cr
#'  Any user-defined \code{models} that are not included in \code{controls_m}
#'  will throw an error. \cr \cr 
#'  If any user-defined \code{controls_model} duplicate any existing controls
#'  for the prefab models or if \code{controls_model} contains any duplicate 
#'  named elements, an error will be thrown unless \code{arg_checks = FALSE},
#'  in which case, the first user-input element for any and all conflicting 
#'  copies is selected. This override allows users to test an existing
#'  prefab model with different configurations, such as on a new data set, 
#'  without requiring a new model. \cr \cr
#'  Users interested in adding models to the prefab set should add the
#'  controls to the \code{prefab_model_controls} non-exported function
#'  found in the \code{prepare_models.R} script.
#'
#' @param models \code{character} vector of name(s) of model(s) whose 
#'  script-writing controls need to be included in the \code{list} used
#'  to write the scripts. 
#'
#' @param controls_model Additional controls for models not in the prefab 
#'  set. \cr 
#'  A \code{list} of a single model's script-writing controls or a
#'  \code{list} of \code{list}s, each of which is a single model's 
#'  script-writing controls. \cr 
#'  Presently, each model's script writing controls should include three 
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
#'  If only a single model is added, the name of the model from the element
#'  \code{name} will be used to name the model's \code{list} in the larger
#'  \code{list}. If multiple models are added, each element \code{list} must
#'  be named according to the model and the \code{name} element. \cr 
#'  See \code{Details} and \code{Examples}. 
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. 
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @return \code{model_controls}: named \code{list} of length equal to
#'  the number of elements in \code{models} and with elements that are each 
#'  \code{list}s of those \code{models}'s script-writing controls. \cr \cr
#'  \code{extract_min_lag}: \code{numeric} value of the minimum non-0 lag
#'  from any included model or \code{NA} if no models have lags. \cr \cr
#'  \code{extract_data_sets}: \code{character} vector of the data set names
#'  from all included models.
#'
#' @examples
#'  model_controls(prefab_models())
#'  controls <- list(name = "xx", data_sets = prefab_data_sets(), 
#'                   interpolate = FALSE, covariatesTF = FALSE, lag = NA)
#'  model_controls("xx", controls)
#'  model_controls(c(prefab_models(), "xx"), controls)
#'  model_controls(c("xx", "ESSS"), controls)
#'  extract_min_lag()
#'  extract_min_lag("AutoArima")
#'  extract_data_sets()
#'  extract_data_sets("AutoArima")
#'
#' @export
#'
model_controls <- function(models = NULL, controls_model = NULL, 
                           quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  if(list_depth(controls_model) == 1){
    controls_model <- list(controls_model)
    names(controls_model) <- controls_model[[1]]$name
  }
  nadd <- length(controls_model)
  prefab_controls <- prefab_model_controls()
  nprefab <- length(prefab_controls)
  for(i in 1:nprefab){
    controls_model[nadd + i] <- list(prefab_controls[[i]])
    names(controls_model)[nadd + i] <- names(prefab_controls)[i]
  }
  missing_controls <- !(models %in% names(controls_model))
  which_missing_controls <- which(missing_controls == TRUE)
  nmissing_controls <- length(which_missing_controls) 
  if(nmissing_controls > 0){
    which_missing <- models[missing_controls]
    all_missing <- paste(which_missing, collapse = ", ")
    msg <- paste0("  ~no controls input for ", all_missing)
    msg <- c(msg, "  **assuming controls follow `model_control()`**")  
    messageq(msg, quiet)
    nadd <- length(controls_model)
    for(i in 1:nmissing_controls){
      mod_name <- models[which_missing_controls[i]]

      controls_model[[nadd + i]] <- model_control(name = mod_name,
                                                   arg_checks = arg_checks)
      names(controls_model)[nadd + i] <- mod_name
    }
  }
  replicates <- table(names(controls_model))
  if(any(replicates > 1) & arg_checks){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of model(s): ", all_conflicting)
    msg2 <- paste0(msg, "\n   to override, set `arg_checks = FALSE`")
    stop(msg2)
  }
  if(any(replicates > 1) & !arg_checks){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of model(s): ", all_conflicting)
    msg2 <- c(msg, " using first user-defined input for each")
    messageq(msg2, quiet)
    umods <- unique(names(controls_model))
    nmods <- length(umods)
    controls_model2 <- vector("list", length = nmods)
    for(i in 1:nmods){
      choice <- which(names(controls_model) == umods[i])[1]
      controls_model2[[i]] <- controls_model[[choice]]
    }
    names(controls_model2) <- umods
    controls_model <- controls_model2
  }

  included_models <- which(names(controls_model) %in% models)
  controls_model[included_models]
}

#' @rdname model_controls
#'
#' @export
#'
extract_min_lag <- function(models = prefab_models(), controls_model = NULL, 
                            quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  controls <- model_controls(models = models,
                             controls_model = controls_model,
                             quiet = quiet, arg_checks = arg_checks)
  nmods <- length(controls)
  lags <- rep(NA, nmods)
  for(i in 1:nmods){
    lag_i <- controls[[i]]$lag
    lags[i] <- ifelse(is.na(lag_i), Inf, lag_i)
  }
  min_lag <- min(lags)
  ifelse(min_lag == Inf, NA, min_lag)
}

#' @rdname model_controls
#'
#' @export
#'
extract_data_sets <- function(models = prefab_models(), 
                              controls_model = NULL, 
                              quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  controls <- model_controls(models = models, 
                             controls_model = controls_model,
                             quiet = quiet, arg_checks = arg_checks)
  nmods <- length(controls)
  data_sets <- NULL
  for(i in 1:nmods){
    data_sets <- unique(c(data_sets, controls[[i]]$data_sets))
  }
  data_sets
}

#' @title Provide the names of the prefab models
#'
#' @description Create a \code{character} vector of the names of the models
#'  to be included including the pre-fabricated (prefab) models, extracting
#'  them from the \code{prefab_model_controls} internal function. \cr \cr
#'  The currently prefabricated models include \code{"AutoArima"}, 
#'  \code{"ESSS"}, \code{"NaiveArima"}, \code{"nbGARCH"}, \code{"nbsGARCH"}, 
#'  \code{"pevGARCH"}. 
#'
#' @return \code{character} vector of model names.
#'
#' @examples
#'  prefab_models()
#'
#' @export
#'
prefab_models <- function(){
  names(prefab_model_controls())
}

#' @title Write the template for a model into model subdirectory
#'
#' @description 
#'  \code{write_model} creates a template script (as written by 
#'  \code{model_template}) for a given model. \cr \cr
#'  \code{model_template} creates the \code{character}-valued
#'  text for a model script to be housed in the model directory, as written
#'  out by \code{write_model}. \cr \cr
#'
#' @param main \code{character} value of the name of the main component of
#'  the directory tree. 
#'
#' @param name \code{character} value of the name of the model.
#'
#' @param control_model \code{list} of model-level controls, including
#'  \code{name}, a \code{character} value of the model's name;
#'  \code{covariatesTF}, a \code{logical} indicator for if the model requires 
#'  covariates; and \code{lag}, a \code{integer} (or integer \code{numeric}) 
#'  lag time used for the covariates or \code{NA} if 
#'  \code{covariatesTF = FALSE}. Only used if the specific valued argument
#'  is \code{NULL}.
#'
#' @param covariatesTF \code{logical} indicator for if the model requires 
#'  covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NULL} if \code{covariatesTF} is \code{FALSE}.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  
#' @param data_sets \code{character} vector of the rodent data set names
#'  that the model is applied to. 
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'  shown.
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.
#'
#' @return \code{write_mode} \code{\link{write}}s the model script out
#'  and returns \code{NULL}. \cr \cr
#'  \code{model_template}: \code{character}-valued text for a model script 
#'  to be housed in the model directory
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'   write_model("AutoArima")
#'   model_template()
#'  }
#'
#' @export
#'
write_model <- function(name = NULL, data_sets = NULL, 
                        covariatesTF = NULL, lag = NULL, main = ".", 
                        control_model = NULL, control_files = files_control(),
                        quiet = FALSE, verbose = TRUE, 
                        arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  name <- ifnull(name, control_model$name)
  prefab_control <- tryCatch(prefab_model_controls()[[name]],
                             error = function(x){NULL})
  control_model <- ifnull(control_model, prefab_control[[name]])
  covariatesTF <- ifnull(covariatesTF, control_model$covariatesTF)
  lag <- ifnull(lag, control_model$lag)
  data_sets <- ifnull(data_sets, control_model$data_sets)
  return_if_null(name)
  if(verbose){
    message(" -Writing model scripts")
  }
  msg <- NULL
  msg1 <- NULL
  if((is.null(covariatesTF) & is.null(lag))){
    msg1 <- paste0("   ~covariatesTF and lag = NULL for ", name)
    msg2 <- "    **assuming covariatesTF = FALSE, lag = 0**"
    msg <- c(msg, msg1, msg2)
    covariatesTF <- FALSE
    lag <- 0
  } 
  if((!is.null(covariatesTF) & covariatesTF && is.null(lag))){
    msg1 <- paste0("    ~lag = NULL for ", name)
    msg2 <- "    **assuming lag = 0**"
    msg <- c(msg, msg1, msg2)
    lag <- 0
  } 
  if((is.null(covariatesTF) & !is.null(lag))){
    if (is.na(lag)){
      msg1 <- paste0("  ~covariatesTF = NULL for ", name)
      msg2 <- "    **assuming covariatesTF = FALSE**"
      msg <- c(msg, msg1, msg2)
      covariatesTF <- FALSE
    } else if (is.numeric(lag)){
      msg1 <- paste0("   ~covariatesTF = NULL for ", name)
      msg2 <- "    **assuming covariatesTF = TRUE**"
      msg <- c(msg, msg1, msg2)
      covariatesTF <- TRUE
    }
  }
  if(is.null(data_sets)){
    msg1 <- paste0("   ~data_sets = NULL for ", name)
    msg2 <- "    **assuming data_sets = prefab_data_sets()**"
    msg <- c(msg, msg1, msg2)
    data_sets <- prefab_data_sets()
  }

  model_file <- paste0(name, ".R")
  mod_path <- file_path(main = main, sub = "models", files = model_file,
                        arg_checks = arg_checks)
  mod_template <- model_template(name = name, data_sets = data_sets, 
                                 covariatesTF = covariatesTF, lag = lag, 
                                 main = main, control_files = control_files,
                                 quiet = quiet, verbose = verbose,
                                 arg_checks = arg_checks)
  if (file.exists(mod_path) & control_files$overwrite){
    verb <- ifelse(verbose, "Updating ", "")
    msgM <- paste0("  -", verb, name)
    write(mod_template, mod_path)
  } else if (!file.exists(mod_path)){
    verb <- ifelse(verbose, "Adding ", "")
    msgM <- paste0("  -", verb, name)
    write(mod_template, mod_path)
  } 
  if(!is.null(msgM)){
    messageq(msg, !verbose)
    messageq(msgM, quiet)
  }
}

#' @rdname write_model
#'
#' @export
#'
model_template <- function(name = NULL, data_sets = NULL,
                           covariatesTF = FALSE, lag = NULL, main = ".", 
                           control_files = files_control(),
                           quiet = FALSE, verbose = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(name)
  data_sets <- ifnull(data_sets,  
                      model_controls(models = name, 
                              arg_checks = arg_checks)[[name]]$data_sets)
  return_if_null(data_sets)
  main_arg <- paste0(', main = "', main, '"')
  control_files_arg <- paste0(', control_files = control_files')
  quiet_arg <- paste0(', quiet = ', quiet)
  verbose_arg <- paste0(', verbose = ', verbose)
  arg_checks_arg <- paste0(', arg_checks = ', arg_checks)
  lag_arg <- NULL
  if (covariatesTF){
    lag_arg <- paste0(', lag = ', lag)
  }
  ds_args <- paste0('data_set = "', data_sets, '"')
  nds <- length(data_sets)
  out <- NULL
  for(i in 1:nds){
    resp <- paste0('cast_', data_sets[i])
    model_args <- paste0(ds_args[i], lag_arg, main_arg, control_files_arg,
                         quiet_arg, verbose_arg, arg_checks_arg) 
    model_fun <- paste0(name, '(', model_args, ');')
    model_line <- paste0(resp, ' <- ', model_fun)
    save_args <- paste0(resp, main_arg, quiet_arg, arg_checks_arg)
    save_fun <- paste0('save_cast_output(', save_args, ');')
    save_line <- save_fun
    newout <- c(model_line, save_line)
    out <- c(out, newout)
  }
  out
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
  messageq("Checking model availability", quiet)
  model_dir <- sub_path(main = main, subs = "models", arg_checks = arg_checks)
  if (!dir.exists(model_dir)){
    stop("Models subidrectory does not exist")
  }
  available <- list.files(model_dir)
  if (models[1] != "all"){
    modelnames <- paste0(models, ".R")
    torun <- (modelnames %in% available)  
    if (any(torun == FALSE)){
      missmod <- paste(models[which(torun == FALSE)], collapse = ", ")
      stop(paste0("Requested model(s) ", missmod, " not in directory \n"))
    }
  }
  messageq(" *All requested models available*", quiet)
  messageq("---------------------------------------------------------", quiet)
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