#' @title Provide the Names or Controls for the Prefab Rodent Datasets
#'
#' @description Create a `character` vector of the names of the pre-fabricated (prefab) rodent datasets or a `list` of their controls
#'
#' @return `prefab_datasets`: `character` vector of dataset names. \cr \cr
#'         `prefab_datasets_controls`: `list` vector of dataset controls. 
#'
#' @name prefabricated_datasets
#'
NULL

#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_datasets <- function( ){

  names(prefab_datasets_controls( ))

}


#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_datasets_controls <- function ( ) {

  prefab_controls_file <- system.file(...     = "extdata", 
                                      ...     = "prefab_datasets_controls.yaml", 
                                      package = "portalcasting")

  read_yaml(prefab_controls_file)

}