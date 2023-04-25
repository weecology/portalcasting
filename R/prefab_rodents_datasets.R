#' @title Provide the Names or Controls for the Prefabricated Rodent Datasets
#'
#' @description Create a `character` vector of the names of the pre-fabricated (prefab) rodent datasets or species or a `list` of their controls
#'
#' @return `prefab_datasets`: `character` vector of dataset names. \cr 
#'         `prefab_datasets_controls`: `list` of dataset controls. \cr 
#'         `prefab_species`: `character` vector of species abbreviations.
#'
#' @name prefabricated rodents datasets
#'
#' @aliases prefab-rodents prefab-datasets prefab-rodents-datasets
#'
#' @examples
#'   prefab_datasets_controls( )
#'   prefab_datasets( )
#'
NULL

#' @rdname prefabricated-rodents-datasets
#'
#' @export
#'
prefab_datasets <- function( ){

  names(prefab_datasets_controls( ))

}

#' @rdname prefabricated-rodents-datasets
#'
#' @export
#'
prefab_datasets_controls <- function ( ) {

  prefab_controls_file <- system.file(...     = "extdata", 
                                      ...     = "prefab_datasets_controls.yaml", 
                                      package = "portalcasting")

  read_yaml(prefab_controls_file)

}

#' @rdname prefabricated-rodents-datasets
#'
#' @export
#'
prefab_species <- function (main = ".") {

  forecasting_species(path  = resources_path(main = main), 
                      total = TRUE)

}