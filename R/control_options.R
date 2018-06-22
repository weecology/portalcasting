#' @title Prepare the predictions options for a portalcasting directory
#'
#' @description Create a list of control options for populating the 
#'   predictions subdirectory
#'
#' @param base name of the base directory where the forecasting 
#'   directory should exist (will be created if it doesn't)
#'
#' @param main name of the portalcasting directory
#'
#' @param subs names of the portalcasting subdirectories
#'
#' @param download_existing_predictions logical indicator is the existing
#'   predictions files should be retrieved from the portalPredictions repo
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @return list of settings controlling the directory structure creeatio
#'
#' @export
#'
predictions_options <- function(base = "~", main = "forecasting", 
                                subs = subdirs(), 
                                download_existing_predictions = TRUE,
                                quiet = FALSE){
  tree <- dirtree(base, main, subs)
  list(tree = tree, 
       download_existing_predictions = download_existing_predictions, 
       quiet = quiet)
}

#' @title Prepare the model options for a portalcasting directory
#'
#' @description Create a list of control options for the model set-up
#'
#' @param base name of the base directory where the forecasting 
#'   directory should exist (will be created if it doesn't)
#'
#' @param main name of the portalcasting directory
#'
#' @param subs names of the portalcasting subdirectories
#'
#' @param quiet logical indicator if progress messages should be quieted
#'
#' @param model names of model scripts to include
#'
#'
#' @return list of model control options
#'
#' @export
#'
models_options <- function(base = "~", main = "forecasting", subs = subdirs(),
                           quiet = FALSE, model = models()){

  tree <- dirtree(base, main, subs)
  list(model = model, quiet = quiet, tree = tree)
}

model_options <- function(base = "~", main = "forecasting", subs = subdirs(),
                          name = "AutoArima", covariates = FALSE, lag = 0,
                          quiet = FALSE){
  tree <- dirtree(base, main, subs)
  list(name = name, covariates = covariates, lag = lag, quiet = quiet, 
       tree = tree)
}

