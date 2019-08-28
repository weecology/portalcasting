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


#' @title Produce the control lists for the model script writing
#' 
#' @description The model scripts are written using a generalized function
#'  that takes a few arguments that vary among models, and so this function
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
#'  If any user-defined \code{controls_m} duplicate any existing controls for
#'  the prefab models or if \code{controls_m} contains any duplicate named
#'  elements, an error will be thrown. \cr \cr
#'  Users interested in adding models to the prefab set should add the
#'  controls to the \code{prefab_model_controls} non-exported function
#'  found in the \code{prepare_models.R} script.
#'
#' @param models \code{character} vector of name(s) of model(s) whose 
#'  script-writing controls need to be included in the \code{list} used
#'  to write the scripts. 
#'
#' @param controls_m Additional controls for models not in the prefab set. 
#'  \cr 
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
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}. 
#'
#' @return \code{model_script_controls}: named \code{list} of length equal to
#'  the number of elements in \code{models} and with elements that are each 
#'  \code{list}s of those \code{models}'s script-writing controls. \cr \cr
#'  \code{extract_min_lag}: \code{numeric} value of the minimum non-0 lag
#'  from any included model or \code{NA} if no models have lags.
#'
#' @examples
#'  model_script_controls(prefab_models())
#'  controls <- list(name = "xx", data_sets = prefab_data_sets(), 
#'                   interpolate = FALSE, covariatesTF = FALSE, lag = NA)
#'  model_script_controls("xx", controls)
#'  model_script_controls(prefab_models("xx"), controls)
#'  model_script_controls(c("xx", "ESSS"), controls)
#'  extract_min_lag()
#'  extract_min_lag("AutoArima")
#'
#' @export
#'
model_script_controls <- function(models = NULL, controls_m = NULL, 
                                  arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(models)
  if(list_depth(controls_m) == 1){
    controls_m <- list(controls_m)
    names(controls_m) <- controls_m[[1]]$name
  }
  nadd <- length(controls_m)
  prefab_controls <- prefab_model_controls()
  nprefab <- length(prefab_controls)
  for(i in 1:nprefab){
    controls_m[nadd + i] <- list(prefab_controls[[i]])
    names(controls_m)[nadd + i] <- names(prefab_controls)[i]
  }
  included_models <- which(names(controls_m) %in% models)
  missing_controls <- which((models %in% names(controls_m)) == FALSE)
  replicates <- table(names(controls_m))
  if(length(missing_controls) > 0){
    which_missing <- models[missing_controls]
    all_missing <- paste(which_missing, collapse = ", ")
    msg <- paste0("missing controls for model(s): ", all_missing)
    stop(msg)
  }
  if(any(replicates > 1)){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of model(s): ", all_conflicting)
    stop(msg)
  }
  controls_m[included_models]
}

#' @rdname model_script_controls
#'
#' @export
#'
extract_min_lag <- function(models = prefab_models(), controls_m = NULL, 
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  controls <- model_script_controls(models = models, controls_m = controls_m,
                                    arg_checks = arg_checks)
  nmods <- length(controls)
  lags <- rep(NA, nmods)
  for(i in 1:nmods){
    lag_i <- controls[[i]]$lag
    lags[i] <- ifelse(is.na(lag_i), Inf, lag_i)
  }
  min_lag <- min(lags)
  ifelse(min_lag == Inf, NA, min_lag)
}

#' @rdname model_script_controls
#'
#' @export
#'
extract_data_sets <- function(models = prefab_models(), controls_m = NULL, 
                            arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  controls <- model_script_controls(models = models, controls_m = controls_m,
                                    arg_checks = arg_checks)
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
#' @param control \code{list} of model-level controls, including
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
#' @param overwrite \code{logical} indicator of whether or not the model
#'   script should be updated.
#'
#' @param arg_checks \code{logical} value of if the arguments should be
#'  checked using standard protocols via \code{\link{check_args}}. The 
#'  default (\code{arg_checks = TRUE}) ensures that all inputs are 
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
                        quiet = FALSE, verbose = TRUE, overwrite = TRUE, 
                        control = NULL, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  name <- ifnull(name, control$name)
  prefab_control <- tryCatch(prefab_model_controls()[[name]],
                             error = function(x){NULL})
  control <- ifnull(control, prefab_control[[name]])
  covariatesTF <- ifnull(covariatesTF, control$covariatesTF)
  lag <- ifnull(lag, control$lag)
  data_sets <- ifnull(data_sets, control$data_sets)
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
  }
  model_file <- paste0(name, ".R")
  mod_path <- file_path(main = main, sub = "models", files = model_file,
                        arg_checks = arg_checks)
  mod_template <- model_template(name = name, data_sets = data_sets, 
                                 covariatesTF = covariatesTF, lag = lag, 
                                 main = main, quiet = quiet,
                                 arg_checks = arg_checks)
  if (file.exists(mod_path) & overwrite){
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
model_template <- function(name = NULL, data_sets = prefab_data_sets(),
                           covariatesTF = FALSE, lag = NULL, main = ".", 
                           quiet = FALSE, arg_checks = TRUE){
  check_args(arg_checks = arg_checks)
  return_if_null(name)
  return_if_null(data_sets)
  main_arg <- paste0(', main = "', main, '"')
  quiet_arg <- paste0(', quiet = ', quiet)
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
    model_args <- paste0(ds_args[i], lag_arg, main_arg, quiet_arg, 
                         arg_checks_arg) 
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
#'  formatted correctly and provides directed error messages if not. \cr
#'  However, in sandboxing, it is often desirable to be able to deviate from 
#'  strict argument expectations. Setting \code{arg_checks = FALSE} triggers
#'  many/most/all enclosed functions to not check any arguments using 
#'  \code{\link{check_args}}, and as such, \emph{caveat emptor}.
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
  model_dir <- sub_paths(main, "models")
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
  messageq("All requested models available", quiet)
  messageq("---------------------------------------------------------", quiet)
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