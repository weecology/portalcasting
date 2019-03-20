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
#' @param model \code{character} value of the name of the model.
#'
#' @param mod_covariates \code{logical} indicator for if the model requires 
#'   covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NULL} if \code{covariates} is \code{FALSE}.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @return \code{model_options}: a class-\code{model_options} \code{list} of 
#'   options used to set up a general model script. 
#'
#' @export
#'
model_options <- function(tree = dirtree(), model = "AutoArima", 
                          mod_covariates = FALSE, lag = NULL, quiet = FALSE){

  check_args()
  list(model = model, mod_covariates = mod_covariates, lag = lag, 
       quiet = quiet, tree = tree) %>%
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
AutoArima_options <- function(tree = dirtree(), model = "AutoArima", 
                              mod_covariates = FALSE, lag = NULL, 
                              quiet = FALSE){
  model_options(tree = tree, model = model, mod_covariates = mod_covariates, 
                lag = lag, quiet = quiet) 
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
ESSS_options <- function(tree = dirtree(),  model = "ESSS", 
                         mod_covariates = FALSE, lag = NULL, quiet = FALSE){
  model_options(tree = tree, model = model, mod_covariates = mod_covariates, 
                lag = lag, quiet = quiet) 
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
nbGARCH_options <- function(tree = dirtree(), model = "nbGARCH", 
                            mod_covariates = FALSE, lag = NULL, 
                            quiet = FALSE){
  model_options(tree = tree, model = model, mod_covariates = mod_covariates, 
                lag = lag, quiet = quiet) 
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
pevGARCH_options <- function(tree = dirtree(), model = "pevGARCH", 
                             mod_covariates = TRUE, lag = 6, quiet = FALSE){
  model_options(tree = tree, model = model, mod_covariates = mod_covariates, 
                lag = lag, quiet = quiet) 
}

#' @rdname model_options
#'
#' @description \code{nbsGARCH_options} creates a \code{model_options} 
#'   \code{list} of control options for the model script controlling the 
#'   \code{\link{nbsGARCH}} model.
#'
#' @return \code{nbsGARCH_options}: a \code{model_options} \code{list} of 
#'   settings controlling the \code{\link{nbsGARCH}} model creation.
#'
#' @export
#'
nbsGARCH_options <- function(tree = dirtree(), model = "nbsGARCH", 
                            mod_covariates = FALSE, lag = NULL, 
                            quiet = FALSE){
  model_options(tree = tree, model = model, mod_covariates = mod_covariates, 
                lag = lag, quiet = quiet) 
}