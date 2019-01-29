#' @title Provide the names of models
#'
#' @description Based on a \code{set}, returns a character vectory of model
#'   names to be included. Currently only support for \code{set = "prefab"}.
#'
#' @param set \code{characher} value of the type of model (currently only 
#'   support for \code{"prefab"}). Use \code{NULL} to build a custom set
#'   from scratch via \code{add}.
#'
#' @param add \code{character} vector of name(s) of model(s) to add to the 
#'   setup by \code{set}.
#'
#' @return \code{models}-class \code{character} vector of model names.
#'
#' @export
#'
models <- function(set = "prefab", add = NULL){
  if (!is.null(set) & !is.character(set)){
    stop("`set` is not NULL or a character")
  }
  if (length(set) > 1){
    stop("`set` can only be of length = 1")
  } 
  out <- NULL
  if (!is.null(set)){
    if (set == "prefab"){
      out <- c("AutoArima", "ESSS", "nbGARCH", "pevGARCH")
    } else{
      stop("`models` not defined for that `set`")
    }
  }
  if (!is.null(add)){
    if (!is.character(add)){
      stop("`add` is not a character")
    }
    out <- c(out, add)
  }
  classy(out, c("character", "models"))
}

#' @title Write the template for a model into model subdirectory
#'
#' @description \code{write_model}: Create template script (as written by 
#'   \code{model_template}) for a given model
#'
#' @param options_model A class-\code{model_options} \code{list} of
#'   options used to set up a general model script. See 
#'   \code{\link{model_options}}.
#'
#' @export
#'
write_model <- function(options_model = model_options()){
  if (!("model_options" %in% class(options_model))){
    stop("`options_model` is not a model_options list")
  }
  model <- options_model$name
  if (is.null(model)){
    return()
  }
  if (!options_model$quiet){
    cat("", paste0("adding ", model, " model to models subdirectory \n"))
  }
  mod_path <- model_path(options_model$tree, model = options_model$name)
  mod_template <- model_template(options_model)
  write(mod_template, mod_path)
}

#' @rdname write_model
#'
#' @description \code{model_template}: create the \code{character}-valued
#'   text for a model script to be housed in the model directory, as written
#'   out by \code{write_model}.
#'
#' @return \code{model_template}: model script text as a single 
#'   \code{character} value.
#'
#' @export
#'
model_template <- function(options_model = model_options()){
  if (!("model_options" %in% class(options_model))){
    stop("`options_model` is not a model_options list")
  }
  tree <- options_model$tree
  if (any(!(tree$subs %in% subdirs()))){
    addl <- which(!(tree$treesubs %in% subdirs()))
    subnames <- paste(tree$subs[addl], collapse = '", "')
    subs <- paste0('subdirs(subs = c("', subnames, '"))')
  } else{
    subs <- "subdirs()"
  }
  name <- options_model$name
  quiet_arg <- paste0("quiet = ", options_model$quiet)

  if (options_model$covariates){
    lag_arg <- paste0(', lag = ', options_model$lag)
    args_a <- paste0('all, covariates, metadata', lag_arg, quiet_arg)
    c_arg <- paste0('controls, covariates, metadata, level = "Controls"')
    args_c <- paste0(c_arg, lag_arg, quiet_arg)
    path_cov <- 'file_path(tree, "data/covariates.csv")'
    covariate_text <- paste0('\ncovariates <- read.csv(', path_cov, '); \n')
  } else{
    args_a <- paste0("all, metadata, ", quiet_arg)
    args_c <- paste0('controls, metadata, level = "Controls", ', quiet_arg)
    covariate_text <- "\n"
  }

  path_a <- 'file_path(tree, "data/all.csv")'
  path_c <- 'file_path(tree, "data/controls.csv")'
  path_m <- 'file_path(tree, "data/metadata.yaml")'

  paste0(
'tree <- dirtree("', tree$base, '", "', tree$main, '", ', subs, ');
all <- read.csv(', path_a, ');
controls <- read.csv(', path_c, ');',
covariate_text, 
'metadata <- yaml::yaml.load_file(', path_m, ');
f_a <- ', name ,'(', args_a, ');
f_c <- ', name ,'(', args_c, ');
save_forecast_output(f_a, f_c, "', 
options_model$name, '", metadata, sub_path(tree, "tmp"))'
)

}