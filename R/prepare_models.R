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
    out <- c("AutoArima", "ESSS", "nbGARCH", "pevGARCH")
  }
  return(out)
}

#' @title Write the template for a model into the model sub directory
#'
#' @description Create template script for a given model
#'
#' @param model the name of the specific model
#'
#' @param options_model model option list
#'
#' @return Nothing
#'
#' @export
#'
write_model <- function(model = NULL, options_model = model_options()){
  if (is.null(model)){
    return()
  }
  if (!options_model$quiet){
    cat("", paste0("adding ", model, " model to models sub-directory \n"))
  }
  mod_path <- model_path(options_model$tree, model = options_model$name)
  mod_template <- model_template(options_model)
  write(mod_template, mod_path)
}

#' @title Create the text to be written out to a script for a model
#'
#' @description Create the text for a model script to be housed in the model 
#'   directory
#'
#' @param options_model list of options for the specific model
#'
#' @return Nothing
#'
#' @export
#'
model_template <- function(options_model = model_options()){
  tree <- options_model$tree
  subnames <- paste(tree$subs, collapse = '", "')
  subs <- paste0('c("', subnames, '")')
  name <- options_model$name

  args_a <- paste('all', 'metadata', sep = ', ')
  args_c <- paste('all', 'metadata', 'level = "Controls"', sep = ', ')

  path_a <- 'file_path(tree, "data/all.csv")'
  path_c <- 'file_path(tree, "data/controls.csv")'
  path_m <- 'file_path(tree, "data/metadata.yaml")'

  paste0(
'tree <- dirtree("', tree$base, '", "', tree$main, '", ', subs, ');
all <- read.csv(', path_a, ');
controls <- read.csv(', path_c, ');
metadata <- yaml::yaml.load_file(', path_m, ');
f_a <- ', name ,'(', args_a, ');
f_c <- ', name ,'(', args_c, ');
save_forecast_output(f_a, f_c, "', options_model$name, '", metadata)'
)

}


