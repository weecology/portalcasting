#' @title Provide the Names or Controls for the Prefab Models
#'
#' @description Create a `character` vector of the names of the pre-fabricated (prefab) models or a `list` of their controls.
#'
#' @return `prefab_models`: `character` vector of model names. \cr \cr
#'         `prefab_model_controls`: `list` of model controls.
#'
#' @name prefabricated models
#'
NULL

#' @rdname prefabricated-models
#'
#' @export
#'
prefab_model_controls <- function( ) {

  prefab_controls_file <- system.file(...     = "extdata", 
                                      ...     = "prefab_model_controls.yaml", 
                                      package = "portalcasting")

  read_yaml(file = prefab_controls_file)

}

#' @rdname prefabricated-models
#'
#' @export
#'
prefab_models <- function( ) {

  names(x = prefab_model_controls( ))

}
