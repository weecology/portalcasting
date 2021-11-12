#' @title Provide the Names of the Prefabricated Models
#'
#' @description Create a \code{character} vector of the names of the models
#'              to be included including the pre-fabricated (prefab) models.
#'              \cr \cr
#'              The currently prefabricated models include \code{"AutoArima"}, 
#'              \code{"ESSS"}, \code{"NaiveArima"}, \code{"nbGARCH"}, 
#'              \code{"nbsGARCH"}, \code{"pevGARCH"}, 
#               \code{"simplexEDM"}, \code{"GPEDM"}, 
#'              and \code{"jags_RW"}. 
#'
#' @return \code{character} vector of model names.
#'
#' @examples
#'  prefab_models()
#'
#' @export
#'
prefab_models <- function () {

  c("AutoArima", "ESSS", "NaiveArima", "nbGARCH", "nbsGARCH", "pevGARCH",
#    "simplexEDM", "GPEDM", 
    "jags_RW")

}


#' @title Write Model Function Script into Directory
#'
#' @description Writes a model's function as a script into the 
#'              defined directory. 
#'              \cr \cr
#'              \code{model} can be input as a \code{character} string,
#'              symbol (backquoted name), or \code{function}, 
#'              as \code{\link{match.fun}}
#'
#' @param main \code{character} value defining the main component of the 
#'              portalcasting directory tree. 
#'
#' @param model \code{character} name of a model function, the \code{function}
#'              itself, or its symbol (backquoted name).
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'              quieted.
#'
#' @param verbose \code{logical} indicator if detailed messages should be
#'                printed.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
write_model <- function (main = ".", 
                         model = NULL, 
                         quiet = FALSE){

  return_if_null(model)

  messageq("   -", model, quiet = quiet)

  FUN <- match.fun(model)
  FUN_char <- format(FUN)
  out <- c(paste0(model, " <- "), FUN_char)

  model_file <- paste0(model, ".R")
  dest <- normalized_file_path(main, "models", model_file, mustWork = FALSE)

  write(out, dest)

  invisible()

}

