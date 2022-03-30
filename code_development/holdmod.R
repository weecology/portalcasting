


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
#'  "pevGARCH", "jags_RW", "jags_logistic")}) 
#'  has its script-writing controls already included internally
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
#'  In addition, some models require additional specific elements: 
#'  \itemize{
#   \item \code{max_E}: \code{integer} (or integer \code{numeric}) 
#    maximum embedding dimension to search amongst for EDM models. See 
#    \code{\link[rEDM]{simplex}} for more information.
#'   \item \code{control_runjags}: \code{list} of arguments passed to 
#'    \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}. 
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
    messageq(msg, quiet = quiet)
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
    stop(msg2, call. = FALSE)
  }
  if(any(replicates > 1) & !arg_checks){
    which_conflicting <- names(replicates)[which(replicates > 1)]
    all_conflicting <- paste(which_conflicting, collapse = ", ")
    msg <- paste0("conflicting copies of model(s): ", all_conflicting)
    msg2 <- c(msg, " using first user-defined input for each")
    messageq(msg2, quiet = quiet)
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

