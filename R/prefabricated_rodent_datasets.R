#' @title Rodent Dataset Controls List
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
#' @description Extract the names of the prefabricated datasets from the \code{\link{rodent_dataset_controls}}. \cr \cr 
#'              The currently prefabricated datasets include \code{"all"}, \code{"all_interp"}, \code{"controls"}, \code{"controls_interp"}, \code{"exclosures"}, \code{"exclosures_interp"}, \code{"dm_controls"}, and \code{"dm_controls_interp"}. 
#'             
#' @return \code{character} names of the data sets.
#'
#' @export
#'
prefabricated_rodent_datasets <- function () {

  envr <- environment()
  data(rodent_dataset_controls, envir = envr)
  names(rodent_dataset_controls)  

}