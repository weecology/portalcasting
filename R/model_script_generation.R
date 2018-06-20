#' @title Provide the names of models
#'
#' @description Based on a \code{type}, returns a character vectory of model
#'   names to be included. Currently only support for 
#'   \code{type = "portalcasting"}.
#'
#' @param type type of model options (currently only support for 
#'   "portalcasting")
#'
#' @return vector of model names
#'
#' @export
#'
models <- function(type = "portalcasting"){
  out <- NULL
  if (type == "portalcasting"){
    out <- c("autoarima", "esss", "nbgarch", "pevgarch")
  }
  return(out)
}

#' @title Write the template for a model into the model sub directory
#'
#' @description Create template script for a given model
#'
#' @param model the name of the specific model
#'
#' @param tree the name tree of the forecasting directory
#'
#' @param quiet logical print steps to console
#'
#' @return Nothing
#'
#' @export
#'
write_model <- function(model = NULL, tree = tree(), quiet = FALSE){
  if (is.null(model)){
    return()
  }
  if (!quiet){
    cat("", paste0("adding ", model, " model to models sub-directory \n"))
  }
  mod_path <- model_path(tree = tree, model = model)
  mod_template <- "hold" #model_template(tree = tree, model = model)
  write(mod_template, mod_path)
}



