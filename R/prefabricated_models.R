#' @title Model Controls List
#'
#' @description \code{list} of the models available for use via \code{\link{prepare_models}}. 
#'
#' @details The currently prefabricated models include \code{"AutoArima"}
#'
#' @format \code{list} of control \code{list}s, each corresponding to a model. 
#'
"model_controls"

#' @title Provide the Names of the Prefabricated Models
#'
#' @description Create a \code{character} vector of the prefabricated models. \cr \cr
#'
#' @return \code{character} vector of model names.
#'
#' @examples
#'  prefab_models()
#'
#' @export
#'
prefabricated_models <- function () {

  envr <- environment()
  data(model_controls, envir = envr)
  names(model_controls)  


}