#' @title Create a covariate model list
#'
#' @description Convenience function for creating covariate model lists
#'
#' @param mod_type \code{character} name for covariate models. Currently only 
#'   \code{"pevGARCH"} is supported.
#'
#' @return \code{list} of covariate model structures
#'
#' @export
#'
covariate_models <- function(mod_type = "pevGARCH"){
  check_args()
  out <- NULL
  if (mod_type == "pevGARCH"){
    out <- list(c("maxtemp", "meantemp", "precipitation", "ndvi"),
                c("maxtemp", "mintemp", "precipitation", "ndvi"),
                c("mintemp", "maxtemp", "meantemp", "precipitation"),
                c("precipitation", "ndvi"),
                c("mintemp", "ndvi"),
                c("mintemp"),
                c("maxtemp"),
                c("meantemp"),
                c("precipitation"),
                c("ndvi"),
                c(NULL))
  }
  out
}