#' @title Rodent Dataset List
#'
#' @description \code{list} of the rodent datasets available for use via \code{\link{prepare_rodents}}. 
#'
#' @details The currently prefabricated datasets include \code{"all"}, \code{"all_interp"}, \code{"controls"}, \code{"controls_interp"}, \code{"exclosures"}, \code{"exclosures_interp"}, \code{"dm_controls"}, and \code{"dm_controls_interp"}. 
#'
#' @format \code{list} of control \code{list}s, each corresponding to a dataset. 
#'
"rodent_dataset_controls"

#' @title Prefabricated Rodent Datasets
#'
#' @description Extract the prefabricated datasets from the \code{\link{rodent_datasets}} data file. The currently prefabricated datasets include \code{"all"}, \code{"all_interp"}, \code{"controls"}, \code{"controls_interp"}, \code{"exclosures"}, \code{"exclosures_interp"}, \code{"dm_controls"}, and \code{"dm_controls_interp"}. 
#'             
#' @return \code{prefab_rodent_datasets_controls}: \code{list} of the data set controls. \cr \cr
#'         \code{prefab_rodent_datasets}: \code{character} names of the data sets.
#'
#' @name prefabricated_rodent_datasets
#'
NULL

#' @rdname prefabricated_rodent_datasets
#'
#' @export
#'
prefab_rodent_dataset_controls <- function () {

  envr <- environment()
  data(rodent_dataset_controls, envir = envr)
  rodent_dataset_controls

}

#' @rdname prefabricated_rodent_datasets
#'
#' @export
#'
prefab_rodent_datasets <- function () {

  names(prefab_rodent_dataset_controls())  

}