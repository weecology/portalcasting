
#' @title Write Model Function Script into Directory
#'
#' @description Writes a model's function as a script into the defined directory for use in forecasting. \cr \cr
#'              \code{model} can be input as a \code{character} string, symbol (backquoted name), or \code{function}, as \code{\link{match.fun}}
#'
#' @param main \code{character} value defining the main component of the portalcasting directory tree. 
#'
#' @param model \code{character} name of a model function, the \code{function} itself, or its symbol (backquoted name).
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
write_model <- function (main  = ".", 
                         model = NULL, 
                         quiet = FALSE){

  return_if_null(model)

  messageq(" - ", model, quiet = quiet)

  write(x    = c(paste0(model, " <- "), format(match.fun(FUN = model))), 
        file = file.path(main, "models", paste0(model, ".R")))

  invisible()

}

