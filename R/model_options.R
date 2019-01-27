#' @title Prepare options for a portalcasting model
#'
#' @description Suite of functions used to generate model-writing options 
#'   lists: \cr \cr \code{model_options} is a basic template that provides
#'   validation of all of the inputs (specific models' functions can thus just 
#'   wrap around this generalized function). Not to be confused with 
#'   \code{\link{models_options}}, which creates the options list for the 
#'   \code{models} subdirectory.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param name \code{character}-valued name of the model (MUST match the 
#'   function name).
#'
#' @param covariates \code{logical} indicator for if the model requires 
#'   covariates.
#'
#' @param lag \code{numeric} integer lag time used for the covariates 
#'   or \code{NULL} if \code{covariates} is \code{FALSE}.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @return \code{model_options}: a class-\code{model_options} \code{list} of 
#'   options used to set up a general model script. 
#'
#' @export
#'
model_options <- function(tree = dirtree(), name = "AutoArima", 
                          covariates = FALSE, lag = NULL, quiet = FALSE){
  if (!("dirtree" %in% class(tree))){
    stop("`tree` is not of class dirtree")
  }
  if (!is.character(name )){
    stop("`name` is not a character")
  }
  if (length(name) > 1){
    stop("`name` can only be of length = 1")
  }
  if (!("logical" %in% class(covariates))){
    stop("`covariates` is not of class logical")
  }
  if (!("logical" %in% class(quiet))){
    stop("`quiet` is not of class logical")
  }
  if (length(lag) > 1){
    stop("`lag` can only be of length = 1")
  }
  if (!is.null(lag)){
    if (!("numeric" %in% class(lag)) & !("integer" %in% class(lag))){
      stop("`lag` is not of class numeric or integer")
    }
    if(lag < 0 | lag %% 1 != 0){
      stop("`lag` is not a non-negative integer")
    }
  }
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree) %>%
  classy(c("model_options", "list"))
}

#' @rdname model_options
#'
#' @description \code{AutoArima_options} creates a \code{model_options} 
#'   \code{list} of control options for the model script controlling the 
#'   \code{\link{AutoArima}} model.
#'
#' @return \code{AutoArima_options}: a \code{model_options} \code{list} of 
#'   settings controlling the \code{\link{AutoArima}} model creation.
#'
#' @export
#'
AutoArima_options <- function(tree = dirtree(), name = "AutoArima", 
                              covariates = FALSE, lag = NULL, quiet = FALSE){
  model_options(tree = tree, name = name, covariates = covariates, lag = lag, 
                quiet = quiet) 
}

#' @rdname model_options
#'
#' @description \code{ESSS_options} creates a \code{model_options} 
#'   \code{list} of control options for the model script controlling the 
#'   \code{\link{ESSS}} model.
#'
#' @return \code{ESSS_options}: a \code{model_options} \code{list} of 
#'   settings controlling the \code{\link{ESSS}} model creation.
#'
#' @export
#'
ESSS_options <- function(tree = dirtree(),  name = "ESSS", covariates = FALSE, 
                         lag = NULL, quiet = FALSE){
  model_options(tree = tree, name = name, covariates = covariates, lag = lag, 
                quiet = quiet) 
}

#' @rdname model_options
#'
#' @description \code{nbGARCH_options} creates a \code{model_options} 
#'   \code{list} of control options for the model script controlling the 
#'   \code{\link{nbGARCH}} model.
#'
#' @return \code{nbGARCH_options}: a \code{model_options} \code{list} of 
#'   settings controlling the \code{\link{nbGARCH}} model creation.
#'
#' @export
#'
nbGARCH_options <- function(tree = dirtree(), name = "nbGARCH", 
                            covariates = FALSE, lag = NULL, quiet = FALSE){
  model_options(tree = tree, name = name, covariates = covariates, lag = lag, 
                quiet = quiet) 
}

#' @rdname model_options
#'
#' @description \code{pevGARCH_options} creates a \code{model_options} 
#'   \code{list} of control options for the model script controlling the 
#'   \code{\link{pevGARCH}} model.
#'
#' @return \code{pevGARCH_options}: a \code{model_options} \code{list} of 
#'   settings controlling the \code{\link{pevGARCH}} model creation.
#'
#' @export
#'
pevGARCH_options <- function(tree = dirtree(), name = "pevGARCH", 
                             covariates = TRUE, lag = 6, quiet = FALSE){
  model_options(tree = tree, name = name, covariates = covariates, lag = lag, 
                quiet = quiet) 
}