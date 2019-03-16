#' @title Provide the names of models
#'
#' @description Based on a \code{set}, returns a character vectory of model
#'   names to be included. Currently only support for \code{set = "prefab"}.
#'
#' @param add \code{character} vector of name(s) of model(s) to add to the 
#'   setup by \code{model_set}.
#'
#' @param model_set \code{characher} value of the type of model (currently 
#'   only support for \code{"prefab"}). Use \code{NULL} to build a custom set
#'   from scratch via \code{add}.
#'
#' @return \code{models}-class \code{character} vector of model names.
#'
#' @export
#'
model_names <- function(model_set = "prefab", add = NULL){
  check_args()
  out <- NULL
  if (!is.null(model_set) && model_set == "prefab"){
    out <- c("AutoArima", "ESSS", "nbGARCH", "pevGARCH")
  } 
  unique(c(out, add))
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
  check_args()
  model <- options_model$model
  if (is.null(model)){
    return()
  }
  msg <- paste0(" adding ", model, " model to models subdirectory")
  messageq(msg, options_model$quiet)
  mod_path <- model_paths(options_model$tree, models = options_model$model)
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
  check_args()
  tree <- options_model$tree
  if (any(!(tree$subs %in% subdirs()))){
    addl <- which(!(tree$subs %in% subdirs()))
    subnames <- paste(tree$subs[addl], collapse = '", "')
    subs <- paste0('subdirs(subs_names = c("', subnames, '")')
  } else{
    subs <- 'subdirs()'
  }
  name <- options_model$model
  quiet_arg <- paste0("quiet = ", options_model$quiet)

  if (options_model$mod_covariates){
    lag_arg <- paste0(', lag = ', options_model$lag, ', ')
    args_a <- paste0('tree, level = "All"', lag_arg, quiet_arg)
    c_arg <- paste0('tree, level = "Controls"')
    args_c <- paste0(c_arg, lag_arg, quiet_arg)
    path_cov <- 'file_paths(tree, "data/covariates.csv")'
    cov_text <- paste0('\ncovariates <- read_data(tree, "covariates"); \n')
  } else{
    args_a <- paste0('tree, level = "All", ', quiet_arg)
    args_c <- paste0('tree, level = "Controls", ', quiet_arg)
    cov_text <- "\n"
  }

  paste0(
'tree <- dirtree("', tree$base, '", "', tree$main, '", ', subs, ');
f_a <- ', name ,'(', args_a, ');
f_c <- ', name ,'(', args_c, ');
save_forecast_output(f_a, f_c, "', 
options_model$name, '", tree)'
)

}