#' @title Provide the Names or Controls for the Prefab Rodent Datasets
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) rodent datasets or a \code{list} of their controls
#'
#' @return \code{prefab_datasets}: \code{character} vector of dataset names. \cr
#'         \code{prefab_dataset_controls}: \code{list} vector of dataset controls. \cr
#'
#' @examples
#'  prefab_datasets( )
#'  prefab_dataset_controls( )
#'
#' @name prefabricated_datasets
#'
NULL

#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_datasets <- function( ){

  names(prefab_dataset_controls( ))

}


#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_dataset_controls <- function ( ) {

  prefab_controls_file <- system.file("extdata", "prefab_dataset_controls.yaml", package = "portalcasting")

  read_yaml(prefab_controls_file)

}