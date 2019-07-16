#' @title Provide the names of model scripts in the models subdirectory
#'
#' @description Lets the user know what model scripts are available.
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @return \code{character} vector of model names.
#'
#' @export
#'
model_scripts <- function(tree = dirtree()){
  base <- tree$base
  main <- tree$main
  sub <- "models"
  dir_path <- normalizePath(file.path(base, main, sub), mustWork = FALSE)
  list.files(dir_path)
}

#' @title Provide the names of models
#'
#' @description Based on model \code{names} or a \code{set}, returns a 
#'   character vector of model names to be included. 
#'
#' @param names \code{character} vector of name(s) of model(s) to add to the 
#'   set created by \code{set}. Defaults to \code{prefab}, which is 
#'   defined internally as
#'   \code{c("AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")}.
#'
#' @param set \code{character} value of the model set(s) to include. Default
#'   value is \code{NULL} which allows for full customization with 
#'   \code{names}. Currently there is only support for the
#'   only support for \code{"prefab"} (AutoArima, ESSS, nbGARCH, nbsGARCH,
#'   and pevGARCH) and \code{"wEnsemble"} (the prefab models plus the basic
#'   ensemble, a specialized case). 
#'
#' @return \code{character} vector of model names.
#'
#' @export
#'
model_names <- function(names = prefab, set = NULL){
  prefab <- c("AutoArima", "ESSS", "nbGARCH", "nbsGARCH", "pevGARCH")
  out <- NULL
  if (!is.null(set)){
    if(set == "prefab"){
      out <- prefab
    }
    if(set == "wEnsemble"){
      out <- c(prefab, "Ensemble")
    }
  } 
  unique(c(out, names))
}

#' @title Write the template for a model into model subdirectory
#'
#' @description 
#'   \code{write_model}: Create template script (as written by 
#'   \code{model_template}) for a given model. \cr \cr
#'   \code{model_template}: create the \code{character}-valued
#'   text for a model script to be housed in the model directory, as written
#'   out by \code{write_model}. \cr \cr
#'
#' @param tree \code{dirtree}-class directory tree list. See 
#'   \code{\link{dirtree}}.
#'
#' @param name \code{character} value of the name of the model.
#'
#' @param covariates \code{logical} indicator for if the model requires 
#'   covariates.
#'
#' @param lag \code{integer} (or integer \code{numeric}) lag time used for the
#'   covariates or \code{NA} if \code{covariates} is \code{FALSE}.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param update \code{logical} indicator of whether or not the predictions
#'   should be updated.
#'
#' @export
#'
write_model <- function(name = "AutoArima", covariates = FALSE,
                        lag = NA, update = TRUE,
                        tree = dirtree(), quiet = FALSE){
  if (is.null(name)){
    return()
  }

  if((is.null(covariates) & is.null(lag))){
    msg1 <- paste0("\n  *info for ", name, " not present in control*")
    msg2 <- "\n  *assuming covariates = FALSE, lag = 0*"
    msg3 <- paste0(msg1, msg2)
    covariates <- FALSE
    lag <- 0
  } else if((!is.null(covariates) & covariates && is.null(lag))){
    msg1 <- paste0("\n  *lag for ", name, " not present in control*")
    msg2 <- "\n  *assuming lag = 0*"
    msg3 <- paste0(msg1, msg2)
    lag <- 0
  } else if((is.null(covariates) & !is.null(lag))){
    msg1 <- paste0("\n  *covariates for ", name, " not present in control*")
    msg2 <- "\n  *assuming covariates = FALSE*"
    msg3 <- paste0(msg1, msg2)
    covariates <- FALSE
  } else{
    msg3 <- NULL
  }

  mod_path <- model_paths(tree, models = name)
  mod_template <- model_template(name, covariates, lag, tree, quiet)
  if (file.exists(mod_path) & update){
    msg4 <- paste0(" updating ", name, " in models subdirectory")
    msg <- paste0(msg4, msg3)
    messageq(msg, quiet)
    write(mod_template, mod_path)
  } else if (!file.exists(mod_path)){
    msg4 <- paste0(" adding ", name, " to models subdirectory")
    msg <- paste0(msg4, msg3)
    messageq(msg, quiet)
    write(mod_template, mod_path)
  } 
}

#' @rdname write_model
#'
#' @export
#'
model_template <- function(name = "AutoArima", covariates = FALSE,
                           lag = NULL, tree = dirtree(), quiet = FALSE){

  if (any(!(tree$subs %in% subdirs()))){
    addl <- which(!(tree$subs %in% subdirs()))
    subnames <- paste(tree$subs[addl], collapse = '", "')
    subs <- paste0('subdirs(subs_names = c("', subnames, '")')
  } else{
    subs <- 'subdirs()'
  }
  quiet_arg <- paste0("quiet = ", quiet)

  if (covariates){
    lag_arg <- paste0(', lag = ', lag, ', ')
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
name, '", tree)'
)

}