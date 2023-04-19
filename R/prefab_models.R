#' @title Provide the Names or Controls for the Prefabricated Models
#'
#' @description Create a `character` vector of the names of the pre-fabricated (prefab) models or a `list` of their controls.
#'
#' @return `prefab_models`: `character` vector of model names. \cr 
#'         `prefab_models_controls`: `list` of model controls.
#'
#' @name prefabricated models
#'
#' @family prefabmodels
#'
#' @examples
#'   prefab_models_controls( )
#'   prefab_models( )
#'
NULL

#' @rdname prefabricated-models
#'
#' @export
#'
prefab_models_controls <- function( ) {

  prefab_controls_file <- system.file(...     = "extdata", 
                                      ...     = "prefab_models_controls.yaml", 
                                      package = "portalcasting")

  read_yaml(file = prefab_controls_file)

}

#' @rdname prefabricated-models
#'
#' @export
#'
prefab_models <- function( ) {

  names(x = prefab_models_controls( ))

}
