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
#'  (\code{c("AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")})
#'  has its script-writing controls already included internally. Users only
#'  need to include controls for non-prefab \code{models}. \cr \cr
#'  Any user-defined \code{models} that are not included in \code{controls}
#'  will throw an error. \cr \cr 
#'  If any user-defined \code{controls} duplicate any existing controls for
#'  the prefab models or if \code{controls} contains any duplicate named
#'  elements, an error will be thrown. 
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
#'  Presently, each model's script writing controls
#'  should include three elements: \code{name} (a \code{character} value of 
#'  the model name), \code{covariatesTF} (a \code{logical} indicator of if the 
#'  model needs covariates), and \code{lag} (an \code{integer}-conformable 
#'  value of the lag to use with the covariates or \code{NA} if 
#'  \code{covariatesTF = FALSE}). \cr 
#'  If only a single model is added, the name of 
#'  the model from the element \code{name} will be used to name the model's
#'  \code{list} in the larger \code{list}. If multiple models are added, each
#'  element \code{list} must be named according to the model and the
#'  \code{name} element. \cr 
#'  See \code{Details} and \code{Examples}.  
#'
#' @return \code{model_script_controls}: named \code{list} of length equal to
#'  the number of elements in \code{models} and with elements that are each 
#'  \code{list}s of those \code{models}'s script-writing controls. \cr \cr
#'  \code{extract_min_lag}: \code{numeric} value of the minimum non-0 lag
#'  from any included model or \code{NA} if no models have lags.
#'
#' @examples
#'  model_script_controls(prefab_models())
#'  controls <- list(name = "xx", covariatesTF = FALSE, lag = NA)
#'  model_script_controls("xx", controls)
#'  model_script_controls(model_names("xx", "prefab"), controls)
#'  model_script_controls(c("xx", "ESSS"), controls)
#'  extract_min_lag()
#'  extract_min_lag("AutoArima")
#'
#' @export
#'
model_script_controls <- function(models = NULL, controls_m = NULL){
  return_if_null(models)
  check_args()
  if(list_depth(controls_m) == 1){
    controls_m <- list(controls_m)
    names(controls_m) <- controls_m[[1]]$name
  }
  nadd <- length(controls_m)

  prefab_controls <- list(
        AutoArima = list(name = "AutoArima", covariatesTF = FALSE, lag = NA), 
        ESSS = list(name = "ESSS", covariatesTF = FALSE, lag = NA), 
        nbGARCH = list(name = "nbGARCH", covariatesTF = FALSE, lag = NA), 
        nbsGARCH = list(name = "nbsGARCH", covariatesTF = FALSE, lag = NA), 
        pevGARCH = list(name = "pevGARCH", covariatesTF = TRUE, lag = 6))
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
extract_min_lag <- function(models = prefab_models(), controls_m = NULL){
  check_args()
  controls <- pass_and_call(model_script_controls)
  nmods <- length(controls)
  lags <- rep(NA, nmods)
  for(i in 1:nmods){
    lag_i <- controls[[i]]$lag
    lags[i] <- ifelse(is.na(lag_i), Inf, lag_i)
  }
  min_lag <- min(lags)
  ifelse(min_lag == Inf, NA, min_lag)
}

#' @title Provide the names of models
#'
#' @description Create a \code{character} vector of the names of the models
#'  to be included. \cr \cr
#'  \code{prefab_models} provides the names for the base pre-loaded models
#'  (\code{c("AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")}). 
#'  \cr \cr
#'  \code{wEnsemble_models} provides the names for the base pre-loaded models
#'  plus the ensemble. 
#'
#' @param models \code{character} vector of name(s) of model(s) to add to the 
#'   set created by \code{set}. 
#'
#' @param set \code{character} value of the model set(s) to include. Default
#'   value is \code{NULL} which allows for full customization with 
#'   \code{models}. Currently there is only support for the
#'   only support for \code{"prefab"} (AutoArima, ESSS, nbGARCH, nbsGARCH,
#'   and pevGARCH) and \code{"wEnsemble"} (the prefab models plus the basic
#'   ensemble, a specialized case). 
#'
#' @return \code{character} vector of model names.
#'
#' @examples
#'  model_names(c("model1", "model2"))
#'  model_names(c("model1", "model2"), "prefab")
#'  prefab_models()
#'  wEnsemble_models()
#'
#' @export
#'
model_names <- function(models = NULL, set = NULL){
  return_if_null(set, models)
  check_args()
  prefab <- c("AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")
  wEnsemble <- c(prefab, "Ensemble")
  out <- switch(set, "prefab" = prefab, "wEnsemble" = wEnsemble)
  unique(c(out, models))
}

#' @rdname model_names
#'
#' @export
#'
prefab_models <- function(){
  model_names(set = "prefab")
}

#' @rdname model_names
#'
#' @export
#'
wEnsemble_models <- function(){
  model_names(set = "wEnsemble")
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
write_model <- function(name = NULL, covariatesTF = NULL,
                        lag = NULL, main = ".", quiet = FALSE, 
                        overwrite = TRUE, control = NULL){
  check_args()
  covariatesTF <- ifnull(covariatesTF, control$covariatesTF)
  lag <- ifnull(lag, control$lag)
  name <- ifnull(name, control$name)
  return_if_null(name)
  if((is.null(covariatesTF) & is.null(lag))){
    msg1 <- paste0("\n  *info for ", name, " is NULL*")
    msg2 <- "\n  *assuming covariatesTF = FALSE, lag = 0*"
    msg3 <- paste0(msg1, msg2)
    covariatesTF <- FALSE
    lag <- 0
  } else if((!is.null(covariatesTF) & covariatesTF && is.null(lag))){
    msg1 <- paste0("\n  *lag for ", name, " is NULL*")
    msg2 <- "\n  *assuming lag = 0*"
    msg3 <- paste0(msg1, msg2)
    lag <- 0
  } else if((is.null(covariatesTF) & !is.null(lag))){
    if (is.na(lag)){
      msg1 <- paste0("\n  *covariatesTF for ", name, " is NULL*")
      msg2 <- "\n  *assuming covariatesTF = FALSE*"
      msg3 <- paste0(msg1, msg2)
      covariatesTF <- FALSE
    } else if (is.numeric(lag)){
      msg1 <- paste0("\n  *covariatesTF for ", name, " is NULL*")
      msg2 <- "\n  *assuming covariatesTF = TRUE*"
      msg3 <- paste0(msg1, msg2)
      covariatesTF <- TRUE
    }
  } else{
    msg3 <- NULL
  }

  mod_path <- model_paths(main, models = name)
  mod_template <- model_template(name, covariatesTF, lag, main, quiet)
  if (file.exists(mod_path) & overwrite){
    msg4 <- paste0(" updating ", name, " in models subdirectory")
    msg <- paste0(msg4, msg3)
    messageq(msg, quiet)
    write(mod_template, mod_path)
  } else if (!file.exists(mod_path)){
    msg4 <- paste0(" adding ", name, " to models subdirectory")
    msg <- paste0(msg4, msg3)
    messageq(msg, quiet)
    write(mod_template, mod_path)
  } 
}

#' @rdname write_model
#'
#' @export
#'
model_template <- function(name = NULL, covariatesTF = FALSE,
                           lag = NULL, main = ".", quiet = FALSE){
  check_args()
  main_arg <- paste0(', main = "', main, '"')
  quiet_arg <- paste0(', quiet = ', quiet)
  if (covariatesTF){
    lag_arg <- paste0(', lag = ', lag)
    args_a <- paste0('level = "All"', lag_arg, main_arg, quiet_arg)
    args_c <- paste0('level = "Controls"', lag_arg, main_arg, quiet_arg)
  } else{
    args_a <- paste0('level = "All"', main_arg, quiet_arg)
    args_c <- paste0('level = "Controls"', main_arg, quiet_arg)
  }


  paste0('f_a <- ', name ,'(', args_a, ');
f_c <- ', name ,'(', args_c, ');
save_cast_output(f_a, f_c, "', name, '"', main_arg, ')'
  )

}
#' @title Verify that models requested to forecast or hindcast with exist
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
                          quiet = FALSE){
  check_args()
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
}


#' @title Create a covariate model list
#'
#' @description Convenience function for creating covariate model 
#'  \code{list}s.
#'
#' @param model \code{character} name for covariate models. Currently only 
#'  \code{"pevGARCH"} is supported.
#'
#' @return \code{list} of covariate model structures.
#'
#' @examples
#'  covariate_models()
#'
#' @export
#'
covariate_models <- function(model = "pevGARCH"){
  check_args()
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