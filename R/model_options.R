
#' @title Prepare options for a portalcasting model
#'
#' @description Suite of functions used to generate model-writing options 
#'   lists: \cr \cr \code{model_options} is a basic template
#'
#' @param tree directory tree
#'
#' @param name character name of the model (matches function name)
#'
#' @param covariates does the model require covariates
#'
#' @param lag lag time used for the covariates (\code{NULL} if 
#'   \code{covariates} is false)
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return \code{model_options}: a list of model options
#'
#' @export
#'
model_options <- function(tree = dirtree(), name = "AutoArima", 
                          covariates = FALSE, lag = NULL, quiet = FALSE){
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree)
}

#' @rdname model_options
#'
#' @description \code{AutoArima_options} creates a list of control options for
#'   the AutoArima model 
#'
#' @return \code{AutoArima_options}: a list of settings controlling the 
#'   AutoArima model creation
#'
#' @export
#'
AutoArima_options <- function(tree = dirtree(), name = "AutoArima", 
                              covariates = FALSE, lag = NULL, quiet = FALSE){
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree)
}

#' @rdname model_options
#'
#' @description \code{ESSS_options} creates a list of control options for
#'   the ESSS model 
#'
#' @return \code{ESSS_options}: a list of settings controlling the 
#'   ESSS model creation
#'
#' @export
#'
ESSS_options <- function(tree = dirtree(),  name = "ESSS", covariates = FALSE, 
                         lag = NULL, quiet = FALSE){
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree)
}

#' @rdname model_options
#'
#' @description \code{nbGARCH_options} creates a list of control options for
#'   the nbGARCH model 
#'
#' @return \code{nbGARCH_options}: a list of settings controlling the 
#'   nbGARCH model creation
#'
#' @export
#'
nbGARCH_options <- function(tree = dirtree(), name = "nbGARCH", 
                            covariates = FALSE, lag = NULL, quiet = FALSE){
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree)
}

#' @rdname model_options
#'
#' @description \code{pevGARCH_options} creates a list of control options for
#'   the pevGARCH model 
#'
#' @return \code{pevGARCH_options}: a list of settings controlling the 
#'   pevGARCH model creation
#'
#' @export
#'
pevGARCH_options <- function(tree = dirtree(), name = "pevGARCH", 
                             covariates = TRUE, lag = 6, quiet = FALSE){
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree)
}