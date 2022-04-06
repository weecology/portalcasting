#' @title Provide the Names or Controls for the Prefab Rodent Datasets
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) rodent datasets or a \code{list} of their controls
#'
#' @param interpolate \code{logical} value indicating if the interpolated data only should be listed (\code{interpolate = TRUE}), if the non-interpolated data only should be listed (\code{interpolate = FALSE}), or if both should be listed (\code{interpolate = NULL}, default).  
#'
#' @return \code{prefab_datasets}: \code{character} vector of dataset names. \cr
#'         \code{prefab_dataset_controls}: \code{list} vector of dataset controls. \cr
#'
#' @examples
#'  prefab_datasets()
#'  prefab_dataset_controls()
#'
#' @name prefabricated_datasets
#'
NULL

#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_datasets <- function(interpolate = NULL){

  names(prefab_dataset_controls(interpolate = interpolate))

}


#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_dataset_controls <- function (interpolate = NULL) {

  prefab_controls_file <- system.file("extdata", "prefab_dataset_controls.yaml", package = "portalcasting")

  out <- read_yaml(prefab_controls_file)

  if (is.null(interpolate)) {

    out

  } else {

    dsnames <- names(out)

    if (interpolate) {

      out[grepl("_interp", dsnames)]

    } else {

      out[!grepl("_interp", dsnames)]

    }

  }

}