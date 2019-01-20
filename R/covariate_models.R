#' @title Create a covariate model list
#'
#' @description Convenience function for creating covariate model lists
#'
#' @param type type name for covariate models. Currently only "pevgarch" is
#'   supported.
#'
#' @return list of covariate model structures
#'
#' @export
#'
covariate_models <- function(type = "pevgarch"){

  out <- NULL

  if (type == "pevgarch"){
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